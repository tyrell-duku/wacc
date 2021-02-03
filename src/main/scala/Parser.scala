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
