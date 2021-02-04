import parsley.Parsley._
import parsley.implicits.{charLift, stringLift}
import parsley.{Parsley, Result}
import parsley.character.{anyChar, char, digit, letter, noneOf, upper}
import parsley.combinator.{many, option}
import parsley.lift.lift2
import Rules._
import parsley.expr.chain

object Parser {
  val baseType: Parsley[BaseType] =
    ("int" #> IntT) <|> ("bool" #> BoolT) <|> ("char" #> CharT) <|> ("string" #> StringT)

  val unaryOp: Parsley[UnOp] =
    ('!' #> Not) <|> ('-' #> Negation) <|> ("len" #> Len) <|> ("ord" #> Ord) <|> ("chr" #> Chr)

  val binaryOp: Parsley[BinOp] =
    ('*' #> Mul) <|> ('/' #> Div) <|> ('%' #> Mod) <|> ('+' #> Plus) <|>
      ('-' #> Sub) <|> ('>' #> GT) <|> (">=" #> GTE) <|> ('<' #> LT) <|>
      ("<=" #> LTE) <|> ("==" #> Equal) <|> ("!=" #> NotEqual) <|> ("&&" #> And) <|> ("||" #> Or)

  val boolLiteral: Parsley[BoolLiter] =
    ("true" #> BoolLiter(true)) <|> ("false" #> BoolLiter(false))

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
}
