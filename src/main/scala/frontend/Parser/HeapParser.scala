package frontend

import frontend.ExprParser.expr
import frontend.Lexer._
import frontend.LiterParser.{identifier, types}
import frontend.Rules.{
  Addr,
  Calloc,
  DerefPtr,
  Expr,
  Malloc,
  MemoryAlloc,
  Realloc,
  SizeOf
}
import parsley.Parsley
import parsley.Parsley._

object HeapParser {
  // '*' '('<expr>')'
  val derefPtr: Parsley[DerefPtr] =
    "*" *> pos <**> expr.map((ptr: Expr) =>
      (p: (Int, Int)) => DerefPtr(ptr, p)
    ) ? "derefrenced pointer"

  // "malloc" '('<expr>')' | "realloc" '('<ident>, <expr>')' | "calloc" '('<expr>, <type>')'
  val memoryAlloc: Parsley[MemoryAlloc] =
    (Malloc("malloc" *> lexer.parens(expr)) <|> ("realloc" *> lexer.parens(
      Realloc(identifier, "," *> expr)
    )) <|> ("calloc" *> lexer.parens(
      Calloc(expr, "," *> expr)
    ))) ? "memory allocation function"

  // "sizeof" '('<type>')'
  val sizeOf: Parsley[SizeOf] =
    SizeOf("sizeof" *> lexer.parens(types)) ? "sizeof function"
}
