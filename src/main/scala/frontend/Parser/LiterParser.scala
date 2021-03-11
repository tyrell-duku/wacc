package frontend

import parsley.Parsley
import parsley.Parsley._
import parsley.combinator.{many, manyN, option}
import parsley.implicits.charLift
import parsley.expr.{Ops, Postfix, Prefix, precedence}
import parsley.character.{noneOf, oneOf}
import parsley.lift.lift2
import frontend.Rules._
import frontend.Lexer._
import java.lang.Integer.parseInt
import java.lang.NumberFormatException
import scala.math.pow

object LiterParser {
  private val overflowErrorMsg = "Integer is not between -2^31 and 2^31 - 1"
  val BinaryBase = 2
  private val MaxBinLength = 31

  // "int" | "bool"  | "char" | "string"
  lazy val baseType: Parsley[BaseType] =
    ("int" #> IntT) <|>
      ("bool" #> BoolT) <|>
      ("char" #> CharT) <|>
      ("string" #> StringT)

  // <base-type> | <array-type> | <pair-type> | <pointer-type>
  val types: Parsley[Type] = precedence[Type](
    baseType ? "base-type" <|> pairType,
    Ops[Type](Postfix)(
      "[]" #> ArrayT ? "array-type",
      "*" #> PtrT ? "pointer-type"
    )
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

  // <oct-liter> "0o" ('0'-'7')+
  val octalInt: Parsley[Int] = lexer.octal

  // <hex-liter> "0x" ('0' - '9' | 'a' - 'f')+
  val hexadecimalInt: Parsley[Int] = lexer.hexadecimal
  
  /* Converts a binary number to denary value. */
  val binToDen: PartialFunction[(Option[IntSign], List[Char]), Int] = {
    case xs if (xs._2.length <= MaxBinLength) =>
      val (sign, bs) = xs
      var num = Integer.parseInt(bs.mkString, BinaryBase)
      sign match {
        case Some(Neg) => -num
        case _ => num
      }
  }

  val binInt: Parsley[Int] =
    (option(intSign) <~> ("0b" <|> "0B") *> manyN(1, '0' <|> '1')).collectMsg(overflowErrorMsg)(binToDen)

  //  <int-sign>? <digit>+  Range[-2^31 < x < 2^31 - 1]
  val intLiter: Parsley[IntLiter] =  IntLiter(
    (option(lookAhead(intSign))
      <~> (binInt <\> (lexer.integer <* notFollowedBy('b'))  <|> (octalInt ?
      "octal (base-8) integer") <|>
        (hexadecimalInt ? "hexadecimal (base-16) integer")))
      .guard(notOverflow, overflowErrorMsg)
      .map((x: (Option[IntSign], Int)) => x._2) ? "number"
  )

  // Determines whether the integer X is within the acceptable range for integers
  def notOverflow(x: (Option[IntSign], Int)): Boolean = {
    val (sign, n) = x
    !(((sign.isEmpty || (sign contains Pos)) && (n < 0)) ||
      ((sign contains Neg) && (n > 0)))
  }

  // "true" | "false"
  val boolLiteral: Parsley[BoolLiter] =
    ((BoolLiter(b = true) <* "true") <|> (BoolLiter(b =
      false
    ) <* "false")) ? "boolean atom"

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
    CharLiter('\'' *> character <* "\'") ? "'character'"

  // '"' <character>* '"'
  val strLiteral: Parsley[StrLiter] =
    StrLiter('\"' *> many(character) <* "\"") ? "\"characters\""

  // "null"
  val pairLiteral: Parsley[PairLiter] = PairLiter() <* "null"
}
