import parsley.Parsley
import parsley.token.{LanguageDef, Lexer, Predicate}
import parsley.character.{char, digit, isWhitespace, letter, upper}

object Lexer {
  val lexer = new Lexer(
    LanguageDef.plain.copy(
      commentLine = "#",
      identStart = parsley.token.Parser(char('_') <|> letter <|> upper),
      identLetter =
        parsley.token.Parser(char('_') <|> letter <|> upper <|> digit),
      keywords = Set("int", "bool", "char", "string", "len", "ord", "chr"),
      space = Predicate(isWhitespace)
    )
  )

  implicit def implicitSymbol(s: String): Parsley[String] = lexer.symbol_(s)
}
