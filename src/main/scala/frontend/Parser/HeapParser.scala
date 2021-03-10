package frontend

import frontend.ExprParser.expr
import frontend.Lexer._
import frontend.LiterParser.{identifier, types}
import frontend.Rules.{Addr, Calloc, DerefPtr, Malloc, MemoryAlloc, Realloc}
import parsley.Parsley
import parsley.Parsley._

object HeapParser {
  // '*' '('<expr>')'
  val derefPtr: Parsley[DerefPtr] = DerefPtr("*" *> lexer.parens(expr))

  // "malloc" '('<expr>')' | "realloc" '('<ident>, <expr>')' | "calloc" '('<expr>, <type>')'
  val memoryAlloc: Parsley[MemoryAlloc] =
    Malloc("malloc" *> lexer.parens(expr)) <|> ("realloc" *> lexer.parens(
      Realloc(identifier, "," *> expr)
    )) <|> ("calloc" *> lexer.parens(Calloc(expr, "," *> types)))

  // '&'<expr>
  val addr: Parsley[Addr] = Addr("&" *> identifier)

}
