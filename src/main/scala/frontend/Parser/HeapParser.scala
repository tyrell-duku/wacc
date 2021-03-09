package frontend

import frontend.ExprParser.expr
import frontend.Lexer._
import frontend.LiterParser.{identifier, types}
import frontend.Rules.{Calloc, DerefPtr, Malloc, MemoryAlloc, Realloc}
import parsley.Parsley
import parsley.Parsley._

object HeapParser {
  val derefPtr: Parsley[DerefPtr] = DerefPtr("*" *> lexer.parens(expr))

  val memoryAlloc: Parsley[MemoryAlloc] =
    Malloc("malloc" *> lexer.parens(expr)) <|> ("realloc" *> lexer.parens(
      Realloc(identifier, "," *> expr)
    )) <|> ("calloc" *> lexer.parens(Calloc(expr, "," *> types)))

}
