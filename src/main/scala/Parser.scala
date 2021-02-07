import Rules.Ident
import parsley.Parsley._
import parsley.Parsley
import parsley.character.{char, digit, letter, noneOf, upper, whitespace}
import parsley.combinator.{many, option}
import parsley.lift.{lift2, lift3}
import Rules._
import parsley.expr.{InfixL, InfixR, Levels, Ops, Postfix, Prefix, precedence}
import parsley.token.{LanguageDef, Lexer}

object Parser {
  val lexer = new Lexer(
    LanguageDef.plain.copy(
      commentLine = "#",
      commentStart = "/*",
      commentEnd = "*/",
      identStart = parsley.token.Parser(char('_') <|> letter <|> upper),
      identLetter =
        parsley.token.Parser(char('_') <|> letter <|> upper <|> digit),
      space = parsley.token.Parser(whitespace)
    )
  )

  implicit def implicitSymbol(s: String): Parsley[String] = lexer.symbol_(s)

  lazy val baseType: Parsley[BaseType] =
    ("int" #> IntT) <|> ("bool" #> BoolT) <|> ("char" #> CharT) <|> ("string" #> StringT)

  val types: Parsley[Type] = precedence[Type, Type](
    baseType <|> pairType,
    Ops[Type](Postfix)("[]" #> OfArrayType) +:
      Levels.empty[Type]
  )

  val pairElemType: Parsley[PairElemType] =
    "pair" #> PairElemPair <|> types.map(PairElemT)

  lazy val pairType: Parsley[PairType] =
    "pair" *> "(" *> lift2(Pair, pairElemType, "," *> pairElemType <* ")")

  val natural = lexer.decimal
  val intSign: Parsley[IntSign] = ("+" #> Pos) <|> ("-" #> Neg)
  val intLiter: Parsley[IntLiter] = lift2(
    (x: Option[IntSign], y: Int) => IntLiter(x, y),
    option(intSign),
    natural
  )

  val boolLiteral: Parsley[BoolLiter] =
    ("true" #> BoolLiter(true)) <|> ("false" #> BoolLiter(false))

  val pairLiteral: Parsley[PairLiter] = "null" #> PairLiter()

  lazy val identifier: Parsley[Ident] = lexer.identifier <#> Ident

  val escapedChar: Parsley[Char] =
    char('0') <|> char('b') <|> char('t') <|> char('n') <|> char('f') <|>
      char('r') <|> char('"') <|> char('\'') <|> char('\\')

  val character: Parsley[Character] = (Escape <#>
    "\\" *> escapedChar) <|> (NormalChar <#> noneOf('\\', '\'', '"'))

  val charLiteral: Parsley[CharLiter] =
    "\'" *> character.map(CharLiter) <* "\'"

  val strLiteral: Parsley[StrLiter] =
    "\"" *> many(character).map(StrLiter) <* "\""

  val expr: Parsley[Expr] = precedence[Expr, Expr](
    intLiter <|> boolLiteral <|> charLiteral <|> strLiteral <|> pairLiteral <|>
      arrayElem <\> identifier <|> ("(" *> expr.map(Parens) <* ")"),
    Ops[Expr](Prefix)(
      "!" #> Not,
      "-" #> Negation,
      "len" #> Len,
      "ord" #> Ord,
      "chr" #> Chr
    ) +:
      Ops[Expr](InfixL)("*" #> Mul, "/" #> Div, "%" #> Mod) +:
      Ops[Expr](InfixL)("+" #> Plus, "-" #> Sub) +:
      Ops[Expr](InfixL)(
        (">=" #> GTE) <\> (">" #> GT),
        ("<=" #> LTE) <\> ("<" #> LT)
      ) +:
      Ops[Expr](InfixL)("==" #> Equal, "!=" #> NotEqual) +:
      Ops[Expr](InfixL)("&&" #> And) +:
      Ops[Expr](InfixL)("||" #> Or) +:
      Levels.empty[Expr]
  )

  val argList: Parsley[ArgList] = (expr <::> many("," *> expr)).map(ArgList)

  lazy val arrayElem: Parsley[ArrayElem] = lift2(
    ArrayElem,
    identifier,
    "[" *> expr <* "]" <::> many("[" *> expr <* "]")
  )

  val pairElem: Parsley[PairElem] =
    ("fst" *> expr.map(Fst)) <|> ("snd" *> expr.map(Snd))

  val arrayLiter: Parsley[ArrayLiter] =
    "[" *> option(expr <::> many("," *> expr)).map(ArrayLiter) <* "]"

  val assignLHS: Parsley[AssignLHS] = arrayElem <\> identifier <|> pairElem

  val assignRHS: Parsley[AssignRHS] =
    ("newpair" *> "(" *> lift2(Newpair, expr, "," *> expr <* ")")) <|>
      ("call" *> lift2(
        Call,
        identifier,
        "(" *> option(argList) <* ")"
      )) <|> pairElem <|> expr <|> arrayLiter

  private val skipStat: Parsley[Stat] = "skip" #> Skip
  private val eqIdent: Parsley[Stat] =
    lift3(EqIdent, types, identifier, "=" *> assignRHS)
  private val eqAssign: Parsley[Stat] =
    lift2(EqAssign, assignLHS, "=" *> assignRHS)
  private val readStat: Parsley[Stat] = "read" *> assignLHS.map(Read)
  private val freeStat: Parsley[Stat] = "free" *> expr.map(Free)
  private val retStat: Parsley[Stat] = "return" *> expr.map(Return)
  private val exitStat: Parsley[Stat] = "exit" *> expr.map(Exit)
  private val printStat: Parsley[Stat] = "print" *> expr.map(Print)
  private val printlnStat: Parsley[Stat] = "println" *> expr.map(PrintLn)
  private val ifStat: Parsley[Stat] =
    "if" *> lift3(If, expr, "then" *> statement, "else" *> statement <* "fi")
  private val whileStat: Parsley[Stat] =
    "while" *> lift2(While, expr, "do" *> statement <* "done")
  private val beginStat: Parsley[Stat] =
    "begin" *> statement.map(Begin) <* "end"

  private lazy val statement
      : Parsley[Stat] = skipStat <|> eqIdent <|> eqAssign <|>
    readStat <\> retStat <|> freeStat <|> exitStat <|> printlnStat <\> printStat <|>
    ifStat <|> whileStat <|> beginStat

  val stat: Parsley[Stat] = precedence[Stat](
    statement,
    Ops(InfixR)(";" #> Seq)
  )
}
