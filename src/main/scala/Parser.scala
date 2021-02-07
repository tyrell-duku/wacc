import parsley.Parsley._
import parsley.Parsley
import parsley.character.{char, digit, letter, noneOf, oneOf, upper, whitespace}
import parsley.combinator.{many, manyN, option}
import parsley.lift.{lift2, lift3, lift4}
import Rules._
import parsley.expr.{InfixL, InfixR, Ops, Postfix, Prefix, precedence}
import parsley.token.{LanguageDef, Lexer}

object Parser {
  val lexer = new Lexer(
    LanguageDef.plain.copy(
      commentLine = "#",
      commentStart = "*/",
      commentEnd = "/*",
      identStart = parsley.token.Parser(char('_') <|> letter <|> upper),
      identLetter =
        parsley.token.Parser(char('_') <|> letter <|> upper <|> digit),
      space = parsley.token.Parser(whitespace)
    )
  )

  implicit def implicitSymbol(s: String): Parsley[String] = lexer.symbol_(s)

  lazy val baseType: Parsley[BaseType] =
    ("int" #> IntT) <|> ("bool" #> BoolT) <|> ("char" #> CharT) <|> ("string" #> StringT)

  val types: Parsley[Type] = precedence[Type](
    baseType <|> pairType,
    Ops[Type](Postfix)("[]" #> OfArrayType)
  )

  val pairElemType: Parsley[PairElemType] =
    "pair" #> PairElemPair <|> types.map(PairElemT)

  lazy val pairType: Parsley[PairType] =
    "pair" *> lexer.parens(
      lift2(Pair, pairElemType, lexer.comma *> pairElemType)
    )

  val natural: Parsley[Int] = lexer.decimal
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

  val escapedChar: Parsley[Char] = oneOf(
    Set('0', 'b', 't', 'n', 'f', 'r', '"', '\'', '\\')
  )

  val character: Parsley[Character] =
    ("\\" *> escapedChar <#> Escape) <|> (noneOf(
      '\\',
      '\'',
      '"'
    ) <#> NormalChar)

  val charLiteral: Parsley[CharLiter] =
    "\'" *> character <* "\'" <#> CharLiter

  val strLiteral: Parsley[StrLiter] =
    "\"" *> many(character) <* "\"" <#> StrLiter

  val expr: Parsley[Expr] = precedence[Expr](
    intLiter <|> boolLiteral <|> charLiteral <|> strLiteral <|> pairLiteral <|>
      arrayElem <\> identifier <|> (lexer.parens(expr) <#> Parens),
    Ops[Expr](Prefix)(
      "!" #> Not,
      "-" #> Negation,
      "len" #> Len,
      "ord" #> Ord,
      "chr" #> Chr
    ),
    Ops[Expr](InfixL)("*" #> Mul, "/" #> Div, "%" #> Mod),
    Ops[Expr](InfixL)("+" #> Plus, "-" #> Sub),
    Ops[Expr](InfixL)(
      (">=" #> GTE) <\> (">" #> GT),
      ("<=" #> LTE) <\> ("<" #> LT)
    ),
    Ops[Expr](InfixL)("==" #> Equal, "!=" #> NotEqual),
    Ops[Expr](InfixL)("&&" #> And),
    Ops[Expr](InfixL)("||" #> Or)
  )

  val argList: Parsley[ArgList] = lexer.commaSep1(expr) <#> ArgList

  lazy val arrayElem: Parsley[ArrayElem] = lift2(
    ArrayElem,
    identifier,
    manyN(1, lexer.brackets(expr))
  )

  val pairElem: Parsley[PairElem] =
    ("fst" *> expr <#> Fst) <|> ("snd" *> expr <#> Snd)

  val arrayLiter: Parsley[ArrayLiter] =
    lexer.brackets(option(lexer.commaSep1(expr))) <#> ArrayLiter

  val assignLHS: Parsley[AssignLHS] = arrayElem <\> identifier <|> pairElem

  val assignRHS: Parsley[AssignRHS] =
    ("newpair" *> lexer.parens(lift2(Newpair, expr, lexer.comma *> expr))) <|>
      ("call" *> lift2(
        Call,
        identifier,
        lexer.parens(option(argList))
      )) <|> pairElem <|> expr <|> arrayLiter

  private val skipStat: Parsley[Stat] = "skip" #> Skip
  private val eqIdent: Parsley[Stat] =
    lift3(EqIdent, types, identifier, "=" *> assignRHS)
  private val eqAssign: Parsley[Stat] =
    lift2(EqAssign, assignLHS, "=" *> assignRHS)
  private val readStat: Parsley[Stat] = "read" *> assignLHS <#> Read
  private val freeStat: Parsley[Stat] = "free" *> expr <#> Free
  private val retStat: Parsley[Stat] = "return" *> expr <#> Return
  private val exitStat: Parsley[Stat] = "exit" *> expr <#> Exit
  private val printStat: Parsley[Stat] = "print" *> expr <#> Print
  private val printlnStat: Parsley[Stat] = "println" *> expr <#> PrintLn
  private val ifStat: Parsley[Stat] =
    "if" *> lift3(If, expr, "then" *> stat, "else" *> stat <* "fi")
  private val whileStat: Parsley[Stat] =
    "while" *> lift2(While, expr, "do" *> stat <* "done")
  private val beginStat: Parsley[Stat] =
    "begin" *> stat <* "end" <#> Begin

  private lazy val statement
      : Parsley[Stat] = skipStat <|> eqIdent <|> eqAssign <|>
    readStat <\> retStat <|> freeStat <|> exitStat <|> printlnStat <\> printStat <|>
    ifStat <|> whileStat <|> beginStat

  lazy val stat: Parsley[Stat] = precedence[Stat](
    statement,
    Ops(InfixR)(";" #> Seq)
  )

  val param: Parsley[Param] = lift2(Param, types, identifier)

  val paramList: Parsley[ParamList] = ParamList <#> lexer.commaSep1(param)

  val func: Parsley[Func] = lift4(
    Func,
    types,
    identifier,
    lexer.parens(option(paramList)),
    "is" *> stat <* "end"
  )
}
