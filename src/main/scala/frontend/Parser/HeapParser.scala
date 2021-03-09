package frontend

import frontend.ExprParser.expr
import frontend.Lexer._
import frontend.Rules.DerefPtr
import parsley.Parsley
import parsley.Parsley._

object HeapParser {
  val derefPtr: Parsley[DerefPtr] = DerefPtr("*" *> lexer.parens(expr))

}
