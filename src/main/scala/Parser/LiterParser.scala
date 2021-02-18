import parsley.Parsley
import parsley.Parsley._
import parsley.combinator.{option, many}
import parsley.implicits.charLift
import parsley.expr.{Ops, Postfix, precedence}
import parsley.character.{noneOf, oneOf}
import parsley.lift.lift2
import Rules._
import Lexer._

object LiterParser {
  // "int" | "bool"  | "char" | "string"
  lazy val baseType: Parsley[BaseType] =
    ("int" #> IntT) <|>
      ("bool" #> BoolT) <|>
      ("char" #> CharT) <|>
      ("string" #> StringT)

  // <base-type> | <array-type> | <pair-type>
  val types: Parsley[Type] = precedence[Type](
    baseType ? "base-type" <|> pairType,
    Ops[Type](Postfix)("[]" #> ArrayT ? "array-type")
  )

  // <base-type> | <array-type> | 'pair'
  val pairElemType: Parsley[PairElemType] =
    ("pair" #> PairElemPair <|> (PairElemT <#> types)) ? "pair-elem-type"

  // "pair" '('<pair-elem-type> ',' <pair-elem-type>')'
  lazy val pairType: Parsley[PairType] =
    ("pair" *> lexer.parens(
      lift2(Pair, pairElemType, "," *> pairElemType)
    )) ? "pair-type"

  // ('_' | 'a' - 'z' | 'A' - 'Z') ('_' | 'a' - 'z' | 'A' - 'Z' | '0' - 9)*
  lazy val identifier: Parsley[Ident] = Ident(lexer.identifier)

  // '+' | '-'
  val intSign: Parsley[IntSign] = ("+" #> Pos) <|> ("-" #> Neg)

  //  <int-sign>? <digit>+  Range[-2^31 < x < 2^31 - 1]
  val intLiter: Parsley[IntLiter] =
    IntLiter((option(lookAhead(intSign)) <~> lexer.integer)
      .guard(notOverflow, "Integer is not between -2^31 and 2^31 - 1")
      .map((x: (Option[IntSign], Int)) => x._2)) ? "number"

  // Determines whether the integer X is within the acceptable range for integers
  def notOverflow(x: (Option[IntSign], Int)): Boolean = {
    val (sign, n) = x
    !(((sign.isEmpty || (sign contains Pos)) && (n < 0)) ||
      ((sign contains Neg) && (n > 0)))
  }

  // "true" | "false"
  val boolLiteral: Parsley[BoolLiter] =
    ((BoolLiter(b = true) <* "true")  <|>  (BoolLiter( b = false) <* "false")) ? "boolean atom"

  // '0' | 'b' | 't' |'n' | 'f' | 'r' |'"' | ''' | '\'
  val escapedChar: Parsley[Char] = oneOf(
    Set('0', 'b', 't', 'n', 'f', 'r', '"', '\'', '\\')
  )

  // any-ASCII-character-except-'\'-'''-'"' | '\' <escaped-char>
  val character: Parsley[Character] =
    (Escape <#> "\\" *> escapedChar ? "escaped character") <|>
      (NormalChar <#> noneOf('\\', '\'', '"')) ? "ASCII character"

  // ''' <character> '''
  val charLiteral: Parsley[CharLiter] =
    CharLiter ('\'' *> character <* "\'") ? "'character'"

  // '"' <character>* '"'
  val strLiteral: Parsley[StrLiter] =
    StrLiter('\"' *> many(character) <* "\"") ? "\"characters\""

  // "null"
  val pairLiteral: Parsley[PairLiter] = PairLiter() <* "null"
}
