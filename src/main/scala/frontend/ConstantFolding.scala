package frontend

import Rules._
// import frontend.LiterParser.notOverflow
import backend.CodeGenerator.getBaseTypeSize
import backend.DefinedFuncs.PreDefinedFuncs

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
      pos: (Line, Col)
  ): Expr = {
    if (checkOverflow(n1, n2, op)) Overflow(pos)
    else IntLiter(op(n1, n2).toInt, pos)
  }

  /* Folds an application of OP on two integer operands, returning the result.
     The result will be null if a runtime error has occurred.
     PRE: no identifiers. */
  private def foldIntOps: PartialFunction[Expr, Expr] = {
    // Base case
    case n: IntLiter => n
    // UnOp folding
    case Negation(IntLiter(n, _), p) => evalConditionally(0, n, (_ - _), p)
    case od @ Ord(e, pos) if !od.containsIdent =>
      val CharLiter(NormalChar(c), _) = fold(e)
      IntLiter(c.toInt, pos)
    case BitwiseNot(IntLiter(n, _), pos) => IntLiter(~n, pos)
    case SizeOf(t, pos)                  => IntLiter(getBaseTypeSize(t), pos)
    // BinOp folding
    case Mul(IntLiter(n1, p), IntLiter(n2, _), _) =>
      evalConditionally(n1, n2, (_ * _), p)
    case Div(IntLiter(n1, p), IntLiter(n2, posError), _) =>
      if (n2 == 0) ZeroDivision(posError)
      else evalConditionally(n1, n2, (_ / _), p)
    case Plus(IntLiter(n1, p), IntLiter(n2, _), _) =>
      evalConditionally(n1, n2, (_ + _), p)
    case Sub(IntLiter(n1, p), IntLiter(n2, _), _) =>
      evalConditionally(n1, n2, (_ - _), p)
    case Mod(IntLiter(n1, p), IntLiter(n2, posError), _) =>
      if (n2 == 0) ZeroDivision(posError)
      else evalConditionally(n1, n2, (_ % _), p)
    case BitwiseAnd(IntLiter(n1, p), IntLiter(n2, _), _) => IntLiter(n1 & n2, p)
    case BitwiseOr(IntLiter(n1, p), IntLiter(n2, _), _) =>
      evalConditionally(n1, n2, (_ | _), p)
    case BitwiseXor(IntLiter(n1, p), IntLiter(n2, _), _) =>
      evalConditionally(n1, n2, (_ ^ _), p)
    case LogicalShiftLeft(IntLiter(n1, p), IntLiter(n2, _), posError) =>
      if (n1 < 0 || n2 < 0) NegShift(posError)
      else evalConditionally(n1, n2, (_ << _), p)
    case LogicalShiftRight(IntLiter(n1, p), IntLiter(n2, _), posError) =>
      if (n1 < 0 || n2 < 0) NegShift(posError)
      else evalConditionally(n1, n2, (_ >> _), p)
    // Recursive case (ArithOps & BitwiseOps)
    case op @ (_: ArithOps | _: BitwiseOps | _: BitwiseNot)
        if !op.containsIdent =>
      foldIntOps(op.map(foldIntOps))
  }

  /* Folds an expression that will evaulate to a boolean. This function fold
      attempt to fold everything and return the result. */
  private def foldBoolOps: PartialFunction[Expr, Expr] = {
    // Base case
    case b: BoolLiter => b
    // UnOp folding
    case Not(BoolLiter(b, _), pos) => BoolLiter(!b, pos)
    // BinOp folding
    case And(BoolLiter(b1, pos), BoolLiter(b2, pos2), _) =>
      BoolLiter(b1 && b2, pos)
    case Or(BoolLiter(b1, pos), BoolLiter(b2, pos2), _) =>
      BoolLiter(b1 || b2, pos)
    // ComparOps folding
    case GT(CharLiter(NormalChar(c1), pos), CharLiter(NormalChar(c2), _), _) =>
      BoolLiter(c1 > c2, pos)
    case GT(IntLiter(n1, pos), IntLiter(n2, _), _) => BoolLiter(n1 > n2, pos)
    case GTE(CharLiter(NormalChar(c1), pos), CharLiter(NormalChar(c2), _), _) =>
      BoolLiter(c1 >= c2, pos)
    case GTE(IntLiter(n1, pos), IntLiter(n2, _), _) => BoolLiter(n1 >= n2, pos)
    case LT(CharLiter(NormalChar(c1), pos), CharLiter(NormalChar(c2), _), _) =>
      BoolLiter(c1 < c2, pos)
    case LT(IntLiter(n1, pos), IntLiter(n2, _), _) => BoolLiter(n1 < n2, pos)
    case LTE(CharLiter(NormalChar(c1), pos), CharLiter(NormalChar(c2), _), _) =>
      BoolLiter(c1 <= c2, pos)
    case LTE(IntLiter(n1, pos), IntLiter(n2, _), _) => BoolLiter(n1 <= n2, pos)
    case eq @ Equal(l, r, pos) if !eq.containsIdent =>
      BoolLiter(fold(l) == fold(r), pos)
    case neq @ NotEqual(l, r, pos) if !neq.containsIdent =>
      BoolLiter(fold(l) != fold(r), pos)
    case op @ (_: Not | _: LogicalOps) if !op.containsIdent =>
      foldBoolOps(op.map(foldBoolOps))
    case op @ (_: ComparOps | _: EqOps) if !op.containsIdent =>
      foldBoolOps(op.map(fold))
  }

  /* Folds an expression that will evaluate to a char literal */
  private def foldCharOps: PartialFunction[Expr, Expr] = {
    case chr @ Chr(e, pos) if !chr.containsIdent =>
      val IntLiter(n, _) = foldIntOps(e)
      CharLiter(NormalChar(n.toChar), pos)
  }

  /* If unable to fold, return original Expr */
  private def id: PartialFunction[Expr, Expr] = { case e =>
    e
  }

  /* Folds any constant expression, composes all fold partial functions to cover
     all cases*/
  val fold = (foldIntOps :: foldBoolOps :: foldCharOps :: id :: Nil)
    .reduceLeft(_ orElse _)

}
