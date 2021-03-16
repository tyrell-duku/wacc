package frontend

import Rules._
import frontend.LiterParser.notOverflow
import backend.CodeGenerator.getBaseTypeSize

object ConstantFolding {

  /* Checks whether either argument X or Y over/underflow or the application of
     a operation OP with X and Y as its arguments over/underflows. */
  def checkOverflow(x: Int, y: Int, op: (Long, Long) => Long): Boolean = {
    var overflown = false
    val xLong = x.toLong
    val yLong = y.toLong
    if (xLong > Integer.MAX_VALUE || xLong < Integer.MIN_VALUE) {
      overflown = true
    }
    if (yLong > Integer.MAX_VALUE || yLong < Integer.MIN_VALUE) {
      overflown = true
    }
    val result = op(xLong, yLong)
    if (result > Integer.MAX_VALUE || result < Integer.MIN_VALUE) {
      overflown = true
    }
    overflown
  }

  private def evalConditionally(
      n1: Int,
      n2: Int,
      op: (Long, Long) => Long,
      pos: (Int, Int)
  ): Expr = {
    if (checkOverflow(n1, n2, op)) null else IntLiter(op(n1, n2).toInt, pos)
  }

  /* Folds an application of OP on two integer operands, returning the result.
     The result will be null if a runtime error has occurred. */
  def foldIntOps(op: Expr): Expr = op match {
    // Base case
    case n: IntLiter => n
    // UnOp folding
    // TODO: overflow
    case Negation(IntLiter(n, _), pos)   => IntLiter(-n, pos)
    case BitwiseNot(IntLiter(n, _), pos) => IntLiter(~n, pos)
    case SizeOf(t, pos)                  => IntLiter(getBaseTypeSize(t), pos)
    // BinOp folding
    case Mul(IntLiter(n1, p), IntLiter(n2, _), _) =>
      evalConditionally(n1, n2, (_ * _), p)
    case Div(IntLiter(n1, p), IntLiter(n2, _), _) =>
      if (n2 == 0) null else evalConditionally(n1, n2, (_ / _), p)
    case Plus(IntLiter(n1, p), IntLiter(n2, _), _) =>
      evalConditionally(n1, n2, (_ + _), p)
    case Sub(IntLiter(n1, p), IntLiter(n2, _), _) =>
      evalConditionally(n1, n2, (_ - _), p)
    case Mod(IntLiter(n1, p), IntLiter(n2, _), _) =>
      evalConditionally(n1, n2, (_ % _), p)
    case BitwiseAnd(IntLiter(n1, p), IntLiter(n2, _), _) =>
      evalConditionally(n1, n2, (_ & _), p)
    case BitwiseOr(IntLiter(n1, p), IntLiter(n2, _), _) =>
      evalConditionally(n1, n2, (_ | _), p)
    case BitwiseXor(IntLiter(n1, p), IntLiter(n2, _), _) =>
      evalConditionally(n1, n2, (_ ^ _), p)
    case LogicalShiftLeft(IntLiter(n1, p), IntLiter(n2, _), _) =>
      if (n1 < 0 || n2 < 0) null else evalConditionally(n1, n2, (_ << _), p)
    case LogicalShiftRight(IntLiter(n1, p), IntLiter(n2, _), _) =>
      if (n1 < 0 || n2 < 0) null else evalConditionally(n1, n2, (_ >> _), p)
    // Recursive case (ArithOps & BitwiseOps)
    case _: ArithOps | _: BitwiseOps | _: UnOp => foldIntOps(op.map(foldIntOps))
    case e                                     => e
  }
}
