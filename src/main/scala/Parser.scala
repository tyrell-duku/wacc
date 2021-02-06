import parsley.Parsley._
import parsley.implicits.{charLift, stringLift}
import parsley.{Parsley, Result}
import parsley.character.{anyChar, char, digit, letter, noneOf, upper}
import parsley.combinator.{many, option}
import parsley.lift.lift2
import Rules._
import parsley.expr.{GOps, Levels, Ops, Postfix, Prefix, InfixL, precedence}

object Parser {
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
    "pair(" *> lift2(Pair, pairElemType, ',' *> pairElemType <* ')')

  val natural: Parsley[Int] =
    digit.foldLeft1[Int](0)((n, d) => n * 10 + d.asDigit)
  val intSign: Parsley[IntSign] = ('+' #> Pos) <|> ('-' #> Neg)
  val intLiter: Parsley[IntLiter] = lift2(
    (x: Option[IntSign], y: Int) => IntLiter(x, y),
    option(intSign),
    natural
  )

  val boolLiteral: Parsley[BoolLiter] =
    ("true" #> BoolLiter(true)) <|> ("false" #> BoolLiter(false))

  val pairLiteral: Parsley[PairLiter] = "null" #> PairLiter()

  lazy val identifier: Parsley[Ident] =
    Ident <#> (('_' <|> letter <|> upper) <::>
      many('_' <|> letter <|> upper <|> digit)).map((x: List[Char]) =>
      x.mkString
    )

  val escapedChar: Parsley[Char] =
    char('0') <|> char('b') <|> char('t') <|> char('n') <|> char('f') <|>
      char('r') <|> char('"') <|> char('\'') <|> char('\\')

  val character: Parsley[Character] = (Escape <#>
    '\\' *> escapedChar) <|> (NormalChar <#> noneOf('\\', '\'', '"'))

  val charLiteral: Parsley[CharLiter] =
    '\'' *> character.map(CharLiter) <* '\''

  val strLiteral: Parsley[StrLiter] =
    '"' *> many(character).map(StrLiter) <* '"'

  val expr: Parsley[Expr] = precedence[Expr, Expr](
    intLiter <|> boolLiteral <|> charLiteral <|> strLiteral <|> pairLiteral <|>
      arrayElem <\> identifier <|> ('(' *> expr.map(Parens) <* ')'),
    Ops[Expr](Prefix)(
      '!' #> Not,
      '-' #> Negation,
      "len" #> Len,
      "ord" #> Ord,
      "chr" #> Chr
    ) +:
      Ops[Expr](InfixL)('*' #> Mul, '/' #> Div, '%' #> Mod) +:
      Ops[Expr](InfixL)('+' #> Plus, '-' #> Sub) +:
      Ops[Expr](InfixL)(
        (">=" #> GTE) <\> ('>' #> GT),
        ("<=" #> LTE) <\> ('<' #> LT)
      ) +:
      Ops[Expr](InfixL)("==" #> Equal, "!=" #> NotEqual) +:
      Ops[Expr](InfixL)("&&" #> And) +:
      Ops[Expr](InfixL)("||" #> Or) +:
      Levels.empty[Expr]
  )

  val argList: Parsley[ArgList] = (expr <::> many(',' *> expr)).map(ArgList)

  val arrayElem: Parsley[ArrayElem] = lift2(
    ArrayElem,
    identifier,
    '[' *> expr <* ']' <::> many('[' *> expr <* ']')
  )

  val skipStat: Parsley[Stat] = "skip" #> Skip
  val freeStat: Parsley[Stat] = ("free" *> expr).map(Free)
  val retStat: Parsley[Stat] = ("return" *> expr).map(Return)
  val exitStat: Parsley[Stat] = ("exit" *> expr).map(Exit)
  val printStat: Parsley[Stat] = ("print" *> expr).map(Print)
  val printlnStat: Parsley[Stat] = ("println" *> expr).map(PrintLn)
  val pairElem: Parsley[PairElem] =
    ("fst" *> expr.map(Fst)) <|> ("snd" *> expr.map(Snd))
  val statement: Parsley[Stat] = skipStat <|> printlnStat <\> printStat <|>
    retStat <|> exitStat <|> freeStat


  val arrayLiter: Parsley[ArrayLiter] =
    '[' *> option(expr <::> many(',' *> expr)).map(ArrayLiter) <* ']'

  val assignLHS: Parsley[AssignLHS] = arrayElem <\> identifier <|> pairElem

  val assignRHS: Parsley[AssignRHS] =
    ("newpair(" *> lift2(Newpair, expr, ',' *> expr <* ')')) <|>
      ("call " *> lift2(
        Call,
        identifier,
        '(' *> option(argList) <* ')'
      )) <|> pairElem <|> expr <|> arrayLiter

  def run(): Unit = {
    println(assignRHS.runParser("call x(call y)"))
  }

}
