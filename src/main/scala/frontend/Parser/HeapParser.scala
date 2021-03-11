package frontend

import frontend.ExprParser.expr
import frontend.Lexer._
import frontend.LiterParser.{identifier, types}
import frontend.Rules.{
  Addr,
  Calloc,
  DerefPtr,
  Malloc,
  MemoryAlloc,
  Realloc,
  SizeOf
}
import parsley.Parsley
import parsley.Parsley._

object HeapParser {
  // '*' '('<expr>')'
  val derefPtr: Parsley[DerefPtr] = DerefPtr(
    "*" *> (lexer.parens(expr) <|> identifier)
  ) ? "derefrenced pointer"

  // "malloc" '('<expr>')' | "realloc" '('<ident>, <expr>')' | "calloc" '('<expr>, <type>')'
  val memoryAlloc: Parsley[MemoryAlloc] =
    (Malloc("malloc" *> lexer.parens(expr)) <|> ("realloc" *> lexer.parens(
      Realloc(identifier, "," *> expr)
    )) <|> ("calloc" *> lexer.parens(
      Calloc(expr, "," *> expr)
    ))) ? "memory allocation function"

  // '&'<expr>
  val addr: Parsley[Addr] =
    Addr("&" *> (lexer.parens(expr) <|> identifier)) ? "address operator"

  // "sizeof" '('<type>')'
  val sizeOf: Parsley[SizeOf] =
    SizeOf("sizeof" *> lexer.parens(types)) ? "sizeof function"
}
