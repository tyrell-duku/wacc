import parsley.Parsley
import parsley.Parsley._
import parsley.character.{char, digit, letter, noneOf, oneOf, upper, whitespace}
import parsley.combinator.{between, many, manyN, option, sepBy1}
import parsley.implicits.charLift
import parsley.lift.{lift2, lift3, lift4}
import Rules._
import parsley.expr.{InfixL, Ops, Postfix, Prefix, precedence}
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
      space = parsley.token.Parser(whitespace.hide)
    )
  )

  implicit def implicitSymbol(s: String): Parsley[String] = lexer.symbol_(s)

  lazy val baseType: Parsley[BaseType] =
    (lexer.keyword("int") #> IntT) <|>
      (lexer.keyword("bool") #> BoolT) <|>
      (lexer.keyword("char") #> CharT) <|>
      (lexer.keyword("string") #> StringT)

  val types: Parsley[Type] = precedence[Type](
    baseType ? "<base-type>" <|> pairType ? "<pair-type>",
    Ops[Type](Postfix)(lexer.keyword("[]") #> ArrayT ? "array-type")
  )

  val pairElemType: Parsley[PairElemType] =
    "pair" #> PairElemPair <|> (PairElemT <#> types) ? "<base-type> or <array-type>"

  lazy val pairType: Parsley[PairType] =
    ("pair" *> lexer.parens(
      lift2(Pair, pairElemType, "," *> pairElemType)
    )) ? "pair(<pair-elem-type>, <pair-elem-type>)"

  val intSign: Parsley[IntSign] = ("+" #> Pos) <|> ("-" #> Neg)
  val intLiter: Parsley[IntLiter] =
    IntLiter <#> (option(lookAhead(intSign)) <~> lexer.integer)
      .guard(notOverflow, "Integer is not between -2^31 and 2^31-1")
      .map((x: (Option[IntSign], Int)) => x._2) ? "number"

  def notOverflow(x: (Option[IntSign], Int)): Boolean = {
    val (sign, n) = x
    if ((sign.isEmpty || (sign contains Pos)) && (n < 0)) {
      return false
    }
    if ((sign contains Neg) && (n > 0)) {
      return false
    }
    true
  }

  val boolLiteral: Parsley[BoolLiter] =
    (("true" #> BoolLiter(true)) <|> ("false" #> BoolLiter(
      false
    ))) ? "boolean atom"

  val pairLiteral: Parsley[PairLiter] = "null" #> PairLiter()

  lazy val identifier: Parsley[Ident] = Ident <#> lexer.identifier

  val escapedChar: Parsley[Char] = oneOf(
    Set('0', 'b', 't', 'n', 'f', 'r', '"', '\'', '\\')
  )

  val character: Parsley[Character] =
    (Escape <#> "\\" *> escapedChar) ? "escaped character" <|>
      (NormalChar <#> noneOf('\\', '\'', '"')) ? "ASCII character"

  val charLiteral: Parsley[CharLiter] =
    (CharLiter <#> '\'' *> character <* "\'") ? "'<character>'"

  val strLiteral: Parsley[StrLiter] =
    (StrLiter <#> '\"' *> many(character) <* "\"") ? "\"" ? "\"<character>*\""

  val expr: Parsley[Expr] = precedence[Expr](
    intLiter <|> boolLiteral <|> charLiteral <|> strLiteral <|> pairLiteral <|>
      arrayElem <\> identifier <|> (Parens <#> lexer.parens(expr)),
    Ops[Expr](Prefix)(
      "!" #> Not ? "unary-operator",
      notFollowedBy(intLiter) *> "-" #> Negation ? "unary-operator",
      "len" #> Len ? "unary-operator",
      "ord" #> Ord ? "unary-operator",
      "chr" #> Chr ? "unary-operator"
    ),
    Ops[Expr](InfixL)(
      "*" #> Mul ? "binary-operator",
      "/" #> Div ? "binary-operator",
      "%" #> Mod ? "binary-operator"
    ),
    Ops[Expr](InfixL)(
      "+" #> Plus ? "binary-operator",
      "-" #> Sub ? "binary-operator"
    ),
    Ops[Expr](InfixL)(
      (">=" #> GTE ? "binary-operator") <\> (">" #> GT ? "binary-operator"),
      ("<=" #> LTE ? "binary-operator") <\> ("<" #> LT ? "binary-operator")
    ),
    Ops[Expr](InfixL)(
      "==" #> Equal ? "binary-operator",
      "!=" #> NotEqual ? "binary-operator"
    ),
    Ops[Expr](InfixL)("&&" #> And ? "binary-operator"),
    Ops[Expr](InfixL)("||" #> Or ? "binary-operator")
  )

  val argList: Parsley[ArgList] =
    ArgList <#> sepBy1(expr, ",") ? "<expr> (',' <expr>)*"

  lazy val arrayElem: Parsley[ArrayElem] = lift2(
    ArrayElem,
    identifier,
    manyN(1, lexer.brackets(expr))
  ) ? "<ident> ([<expr>])+"

  val pairElem: Parsley[PairElem] =
    (Fst <#> "fst" *> expr ? "fst <expr>") <|> (Snd <#> "snd" *> expr ? "snd <expr>")

  val arrayLiter: Parsley[ArrayLiter] = ArrayLiter <#> lexer.brackets(
    option(sepBy1(expr, ","))
  ) ? "[(<expr> (',' <expr>)*)?]"

  val assignLHS: Parsley[AssignLHS] = pairElem <|> arrayElem <\> identifier

  val assignRHS: Parsley[AssignRHS] =
    ("newpair" *> lexer
      .parens(
        lift2(Newpair, expr, "," *> expr)
      )) <|>
      ("call" *> lift2(
        Call,
        identifier,
        lexer.parens(option(argList))
      )) <|> pairElem <|> expr <|> arrayLiter

  private val skipStat: Parsley[Stat] = "skip" #> Skip
  private val eqIdent: Parsley[Stat] =
    lift3(
      EqIdent,
      types,
      identifier,
      "=" *> assignRHS
    ) ? "<type> <ident> = <assign-rhs>"
  private val eqAssign: Parsley[Stat] =
    lift2(EqAssign, assignLHS, "=" *> assignRHS) ? "<assign-lhs> = <assign-rhs>"
  private val readStat: Parsley[Stat] =
    Read <#> "read" *> assignLHS ? "read <assign-lhs>"
  private val freeStat: Parsley[Stat] = Free <#> "free" *> expr ? "free <expr>"
  private val retStat: Parsley[Stat] =
    Return <#> "return" *> expr ? "return <expr>"
  private val exitStat: Parsley[Stat] = Exit <#> "exit" *> expr ? "exit <expr>"
  private val printStat: Parsley[Stat] =
    Print <#> "print" *> expr ? "print <expr>"
  private val printlnStat: Parsley[Stat] =
    PrintLn <#> "println" *> expr ? "println <expr>"
  private val ifStat: Parsley[Stat] =
    "if" *> lift3(
      If,
      expr,
      "then" *> stat,
      between("else", "fi", stat)
    ) ? "if <expr> then <stat> else <stat> fi"
  private val whileStat: Parsley[Stat] =
    "while" *> lift2(
      While,
      expr,
      between("do", "done", stat)
    ) ? "while <expr> do <stat> done"
  private val beginStat: Parsley[Stat] =
    between("begin", "end", Begin <#> stat) ? "begin <stat> end"

  private lazy val statement: Parsley[Stat] = skipStat <|> readStat <\>
    retStat <|> freeStat <|> exitStat <|> printlnStat <\> printStat <|>
    ifStat <|> whileStat <|> beginStat <|> eqIdent <|> eqAssign

  lazy val stat: Parsley[Stat] =
    (statement <* notFollowedBy(";")) <\> (Seq <#> sepBy1(statement, ";"))

  val param: Parsley[Param] = lift2(Param, types, identifier) ? "<type> <ident>"

  val paramList: Parsley[ParamList] =
    ParamList <#> sepBy1(param, ",") ? "<param> (, <param>)*"

  private def statTerminates(stat: Stat): Boolean = stat match {
    case If(_, s1, s2)       => statTerminates(s1) && statTerminates(s2)
    case While(_, s)         => statTerminates(s)
    case Begin(s)            => statTerminates(s)
    case Seq(s)              => statTerminates(s.last)
    case Exit(_) | Return(_) => true
    case _                   => false
  }

  private def funcMsg(f: Func): String = f match {
    case Func(_, Ident(s), _, _) =>
      "Function " + s + " is not ended with return or exit statement"
  }

  val func: Parsley[Func] = lift4(
    Func,
    types,
    identifier,
    lexer.parens(option(paramList)),
    between(
      "is",
      "end",
      stat
    ) ? "<type> <ident> (<param-list>?) is <stat> end"
  ).guard((x: Func) => statTerminates(x.s), (x: Func) => funcMsg(x))

  val program: Parsley[Program] =
    between(
      "begin",
      "end",
      lift2(Program, many(attempt(func)), stat)
    ) ? "begin <func>* <stat> end"
}
