package frontend

import Rules._
import backend.CodeGenerator.getBaseTypeSize

object ConstantFolding {

  private def foldIntOps(op: Expr): Expr = op match {
    // Base case
    case n: IntLiter => n
    // Folding
    case Mul(IntLiter(n1, p), IntLiter(n2, _), _)        => IntLiter(n1 * n2, p)
    case Div(IntLiter(n1, p), IntLiter(n2, _), _)        => IntLiter(n1 / n2, p)
    case Plus(IntLiter(n1, p), IntLiter(n2, _), _)       => IntLiter(n1 + n2, p)
    case Sub(IntLiter(n1, p), IntLiter(n2, _), _)        => IntLiter(n1 - n2, p)
    case Mod(IntLiter(n1, p), IntLiter(n2, _), _)        => IntLiter(n1 % n2, p)
    case BitwiseAnd(IntLiter(n1, p), IntLiter(n2, _), _) => IntLiter(n1 & n2, p)
    case BitwiseOr(IntLiter(n1, p), IntLiter(n2, _), _)  => IntLiter(n1 | n2, p)
    case BitwiseXor(IntLiter(n1, p), IntLiter(n2, _), _) => IntLiter(n1 ^ n2, p)
    case LogicalShiftLeft(IntLiter(n1, p), IntLiter(n2, _), _) =>
      IntLiter(n1 << n2, p)
    case LogicalShiftRight(IntLiter(n1, p), IntLiter(n2, _), _) =>
      IntLiter(n1 >> n2, p)
    // Recursive case (ArithOps & BitwiseOps)
    case _ => foldIntOps(op.map(foldIntOps))
  }

  def foldExpr(e: Expr): Expr = e match {
    // case DerefPtr(ptr, pos)                   =>
    // case Addr(ptr, pos)                       =>
    // case Len(e, pos)                          =>
    // case BitwiseNot(e, pos)                   =>
    // case Equal(lExpr, rExpr, pos)             =>
    // case NotEqual(lExpr, rExpr, pos)          =>
    // case BitwiseAnd(lExpr, rExpr, pos)        =>
    // case BitwiseOr(lExpr, rExpr, pos)         =>
    // case BitwiseXor(lExpr, rExpr, pos)        =>
    // case LogicalShiftLeft(lExpr, rExpr, pos)  =>
    // case LogicalShiftRight(lExpr, rExpr, pos) =>
    case op: SizeOf =>
      val (num, b) = foldInt(op)
      if (b) IntLiter(num, null) else op
    case op: Ord =>
      val (num, b) = foldInt(op)
      if (b) IntLiter(num, null) else op
    case op: Negation =>
      val (num, b) = foldInt(op)
      if (b) IntLiter(num, null) else op
    case op: ArithOps =>
      val (num, b) = foldInt(op)
      if (b) IntLiter(num, null) else op
    case e: Expr => e
  }

  def foldBool(e: Expr): (Boolean, Boolean) = e match {
    case BoolLiter(b, _) => (b, true)
    case And(l, r, _) =>
      val (n1, b1) = foldBool(l)
      val (n2, b2) = foldBool(r)
      (n1 && n2, b1 && b2)
    case Or(l, r, _) =>
      val (n1, b1) = foldBool(l)
      val (n2, b2) = foldBool(r)
      (n1 || n2, b1 && b2)
    // case Equal(l, r, _)    => foldBool(l) == foldBool(r)
    // case NotEqual(l, r, _) => foldBool(l) != foldBool(r)
    case GT(l, r, _) =>
      l.getType(null) match {
        case CharT =>
          val (c1, b1) = foldChar(l)
          val (c2, b2) = foldChar(r)
          (c1 > c2, b1 && b2)
        case IntT =>
          val (n1, b1) = foldInt(l)
          val (n2, b2) = foldInt(r)
          (n1 > n2, b1 && b2)
        case _ => ???
      }
    case GTE(l, r, _) =>
      l.getType(null) match {
        case CharT =>
          val (c1, b1) = foldChar(l)
          val (c2, b2) = foldChar(r)
          (c1 >= c2, b1 && b2)
        case IntT =>
          val (n1, b1) = foldInt(l)
          val (n2, b2) = foldInt(r)
          (n1 >= n2, b1 && b2)
        case _ => ???
      }
    case LT(l, r, _) =>
      l.getType(null) match {
        case CharT =>
          val (c1, b1) = foldChar(l)
          val (c2, b2) = foldChar(r)
          (c1 < c2, b1 && b2)
        case IntT =>
          val (n1, b1) = foldInt(l)
          val (n2, b2) = foldInt(r)
          (n1 < n2, b1 && b2)
        case _ => ???
      }
    case LTE(l, r, _) =>
      l.getType(null) match {
        case CharT =>
          val (c1, b1) = foldChar(l)
          val (c2, b2) = foldChar(r)
          (c1 <= c2, b1 && b2)
        case IntT =>
          val (n1, b1) = foldInt(l)
          val (n2, b2) = foldInt(r)
          (n1 <= n2, b1 && b2)
        case _ => ???
      }
    case Not(b, _) =>
      val (n1, b1) = foldBool(b)
      (!n1, b1)
    case _ => (false, false)
  }

  def foldInt(e: Expr): (Int, Boolean) = e match {
    case IntLiter(n, _) => (n, true)
    case Plus(l, r, _) =>
      val (n1, b1) = foldInt(l)
      val (n2, b2) = foldInt(r)
      (n1 + n2, b1 && b2)
    case Sub(l, r, _) =>
      val (n1, b1) = foldInt(l)
      val (n2, b2) = foldInt(r)
      (n1 - n2, b1 && b2)
    case Div(l, r, _) =>
      val (n1, b1) = foldInt(l)
      val (n2, b2) = foldInt(r)
      (n1 / n2, b1 && b2)
    case Mul(l, r, _) =>
      val (n1, b1) = foldInt(l)
      val (n2, b2) = foldInt(r)
      (n1 * n2, b1 && b2)
    case Negation(e, _) =>
      val (n1, b1) = foldInt(e)
      (-n1, b1)
    case Mod(l, r, _) =>
      val (n1, b1) = foldInt(l)
      val (n2, b2) = foldInt(r)
      (n1 % n2, b1 && b2)
    case SizeOf(t, _) =>
      (getBaseTypeSize(t), true)
    case Ord(e, _) =>
      val (c, b1) = foldChar(e)
      (c.toInt, b1)
    case _ => (0, false)
  }

  def foldChar(e: Expr): (Char, Boolean) = e match {
    case CharLiter(NormalChar(c), _) => (c, true)
    case Chr(e, _) =>
      val (n, b1) = foldInt(e)
      (n.toChar, b1)
    case _ => ('a', false)
  }

}
