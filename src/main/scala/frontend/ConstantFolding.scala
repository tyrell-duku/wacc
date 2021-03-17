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
     The result will be null if a runtime error has occurred.
     PRE: no identifiers. */
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
    case BitwiseAnd(IntLiter(n1, p), IntLiter(n2, _), _) => IntLiter(n1 & n2, p)
    case BitwiseOr(IntLiter(n1, p), IntLiter(n2, _), _) =>
      evalConditionally(n1, n2, (_ | _), p)
    case BitwiseXor(IntLiter(n1, p), IntLiter(n2, _), _) =>
      evalConditionally(n1, n2, (_ ^ _), p)
    case LogicalShiftLeft(IntLiter(n1, p), IntLiter(n2, _), _) =>
      if (n1 < 0 || n2 < 0) null else evalConditionally(n1, n2, (_ << _), p)
    case LogicalShiftRight(IntLiter(n1, p), IntLiter(n2, _), _) =>
      if (n1 < 0 || n2 < 0) null else evalConditionally(n1, n2, (_ >> _), p)
    // Recursive case (ArithOps & BitwiseOps)
    case _: ArithOps | _: BitwiseOps | _: BitwiseNot => foldIntOps(op.map(foldIntOps))
    case e                                     => e
  }

   /* Folds an expression that will evaulate to a boolean. This function fold 
      attempt to fold everything and return the result. */
  private def foldBoolOps(op: Expr): Expr = op match {
    // Base case
    case b : BoolLiter => b
    // UnOp folding
    case Not(BoolLiter(b, _), pos) => BoolLiter(!b, pos)
    // BinOp folding
    case And(BoolLiter(b1, pos), BoolLiter(b2, pos2)) => BoolLiter(b1 && b2, pos)
    case Or(BoolLiter(b1, pos), BoolLiter(b2, pos2)) => BoolLiter(b1 || b2, pos)
    // ComparOps folding
    case GT(CharLiter(c1, pos), CharLiter(c2, _)) => BoolLiter(c1 < c2, pos)
    case GT(IntLiter(n1, pos), IntLiter(n2, _)) => BoolLiter(n1 < n2, pos)
    case gt: GT => foldBoolOps(gt.map(foldIntOps))
    case e => e
  }
  
}
