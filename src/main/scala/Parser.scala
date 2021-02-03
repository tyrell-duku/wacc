import parsley.Parsley._
import parsley.implicits.{charLift, stringLift}
import parsley.{Parsley, Result}
import parsley.character.{anyChar, char, digit, letter, noneOf, upper}
import parsley.combinator.{many, option}
import parsley.lift.lift2
import Rules._
import parsley.expr.chain

object Parser {
  val unaryOp: Parsley[UnOp] =
    ('!' #> Not) <|> ('-' #> Negation) <|> ("len" #> Len) <|> ("ord" #> Ord) <|> ("chr" #> Chr)

}
