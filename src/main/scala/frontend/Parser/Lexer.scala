import parsley.Parsley
import parsley.token.{LanguageDef, Lexer, Predicate}
import parsley.character.{char, digit, isWhitespace, letter, upper}
import scala.language.implicitConversions

object Lexer {

  val keywords = Set(
    "int",
    "bool",
    "char",
    "string",
    "[]",
    "len",
    "ord",
    "chr",
    "fst",
    "snd",
    "skip",
    "read",
    "free",
    "return",
    "exit",
    "print",
    "println",
    "if",
    "then",
    "else",
    "fi",
    "while",
    "do",
    "done",
    "begin",
    "end",
    "call"
  )

  val lexer = new Lexer(
    LanguageDef.plain.copy(
      commentLine = "#",
      identStart = parsley.token.Parser(char('_') <|> letter <|> upper),
      identLetter =
        parsley.token.Parser(char('_') <|> letter <|> upper <|> digit),
      keywords = this.keywords,
      space = Predicate(isWhitespace)
    )
  )

  implicit def implicitSymbol(s: String): Parsley[_] = {
    if (keywords.apply(s)) {
      return lexer.keyword(s)
    }
    lexer.symbol_(s)
  }
}
