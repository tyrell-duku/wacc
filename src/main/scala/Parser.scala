import parsley.Parsley
import parsley.Parsley._
import parsley.character.{char, digit, letter, noneOf, oneOf, upper, whitespace}
import parsley.combinator.{between, many, manyN, option}
import parsley.implicits.charLift
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
    (lexer.keyword("int") #> IntT) <|> (lexer.keyword("bool") #> BoolT) <|>
      (lexer.keyword("char") #> CharT) <|> (lexer.keyword("string") #> StringT)

  val types: Parsley[Type] = precedence[Type](
    baseType <|> pairType,
    Ops[Type](Postfix)(lexer.keyword("[]") #> OfArrayType)
  )

  val pairElemType: Parsley[PairElemType] =
    "pair" #> PairElemPair <|> (PairElemT <#> types)

  lazy val pairType: Parsley[PairType] =
    "pair" *> lexer.parens(
      lift2(Pair, pairElemType, lexer.comma *> pairElemType)
    )

  val intLiter: Parsley[IntLiter] = IntLiter <#> lexer.integer

  val boolLiteral: Parsley[BoolLiter] =
    ("true" #> BoolLiter(true)) <|> ("false" #> BoolLiter(false))

  val pairLiteral: Parsley[PairLiter] = "null" #> PairLiter()

  lazy val identifier: Parsley[Ident] = Ident <#> lexer.identifier

  val escapedChar: Parsley[Char] = oneOf(
    Set('0', 'b', 't', 'n', 'f', 'r', '"', '\'', '\\')
  )

  val character: Parsley[Character] = (Escape <#> "\\" *> escapedChar) <|>
    (NormalChar <#> noneOf('\\', '\'', '"'))

  val charLiteral: Parsley[CharLiter] =
    CharLiter <#> '\'' *> character <* "\'"

  val strLiteral: Parsley[StrLiter] =
    StrLiter <#> '\"' *> many(character) <* "\""

  val expr: Parsley[Expr] = precedence[Expr](
    intLiter <|> boolLiteral <|> charLiteral <|> strLiteral <|> pairLiteral <|>
      arrayElem <\> identifier <|> (Parens <#> lexer.parens(expr)),
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

  val argList: Parsley[ArgList] = ArgList <#> lexer.commaSep1(expr)

  lazy val arrayElem: Parsley[ArrayElem] = lift2(
    ArrayElem,
    identifier,
    manyN(1, lexer.brackets(expr))
  )

  val pairElem: Parsley[PairElem] =
    ("fst" *> expr <#> Fst) <|> ("snd" *> expr <#> Snd)

  val arrayLiter: Parsley[ArrayLiter] =
    lexer.brackets(option(lexer.commaSep1(expr))) <#> ArrayLiter

  val assignLHS: Parsley[AssignLHS] = pairElem <|> arrayElem <\> identifier

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
  private val readStat: Parsley[Stat] = Read <#> "read" *> assignLHS
  private val freeStat: Parsley[Stat] = Free <#> "free" *> expr
  private val retStat: Parsley[Stat] = Return <#> "return" *> expr
  private val exitStat: Parsley[Stat] = Exit <#> "exit" *> expr
  private val printStat: Parsley[Stat] = Print <#> "print" *> expr
  private val printlnStat: Parsley[Stat] = PrintLn <#> "println" *> expr
  private val ifStat: Parsley[Stat] =
    "if" *> lift3(If, expr, "then" *> stat, between("else", "fi", stat))
  private val whileStat: Parsley[Stat] =
    "while" *> lift2(While, expr, between("do", "done", stat))
  private val beginStat: Parsley[Stat] =
    between("begin", "end", Begin <#> stat)

  private lazy val statement: Parsley[Stat] = skipStat <|> readStat <\>
    retStat <|> freeStat <|> exitStat <|> printlnStat <\> printStat <|>
    ifStat <|> whileStat <|> beginStat <|> eqIdent <|> eqAssign

  lazy val stat: Parsley[Stat] = precedence[Stat](
    statement,
    Ops(InfixR)(lexer.semi #> Seq)
  )

  val param: Parsley[Param] = lift2(Param, types, identifier)

  val paramList: Parsley[ParamList] = ParamList <#> lexer.commaSep1(param)

  private def statTerminates(stat: Stat): Boolean = stat match {
    case If(_, s1, s2)        => statTerminates(s1) && statTerminates(s2)
    case While(_, s)          => statTerminates(s)
    case Begin(s)             => statTerminates(s)
    case Seq(_, s)            => statTerminates(s)
    case Exit(_) | Return (_) => true
    case _                    => false
  }

  val func: Parsley[Func] = lift4(
    Func,
    types,
    identifier,
    lexer.parens(option(paramList)),
    between("is", "end", stat.filter(statTerminates))
  )

  val program: Parsley[Program] =
    between("begin", "end", lift2(Program, many(attempt(func)), stat))
}
