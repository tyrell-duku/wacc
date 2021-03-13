package frontend

import Rules._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import backend.CodeGenerator.getExprType
import ConstantFolding.foldExpr

object SSA {
  // <Identifier's name, (LHS unique identifier, RHS unique identifier)>
  private var dict = new HashMap[String, (Int, Int)]
  private var kvs = new HashMap[String, Expr]
  val INITIAL_LHS = 0
  val INITIAL_RHS = 0

  /* If the hashmap doesn't contain the identifier STR, then it is added to
     the hashmap with initial values (1, 1). If the hashmap contains the
     identifier STR, then the values are updated for unique identifying. */
  private def addToHashMap(id: Ident, rhs: AssignRHS): Ident = {
    val Ident(str, pos) = id
    var (x, y) = dict.getOrElse(str, (INITIAL_LHS, INITIAL_RHS))
    x += 1
    y += 1
    dict += ((str, (x, y)))
    val v = x.toString + str
    rhs match {
      case e: Expr => kvs += ((v, transformExpr(e)))
      case _       =>
    }
    Ident(v, pos)
  }

  /* Updates an identifier, prepending the RHS unique ID.
     PRE: the identifier is already in the hashmap DICT. */
  private def updateIdent(id: Ident): Ident = {
    val Ident(str, pos) = id
    val (_, y) = dict(str)
    Ident(y.toString + str, pos)
  }

  private def transformExpr(e: Expr): Expr = e match {
    case DerefPtr(ptr, pos) => DerefPtr(transformExpr(ptr), pos)
    case Addr(ptr, pos)     => Addr(transformExpr(ptr), pos)
    case sizeof: SizeOf     => sizeof
    case Not(e, pos)        => Not(transformExpr(e), pos)
    case Negation(e, pos)   => Negation(transformExpr(e), pos)
    case Len(e, pos)        => Len(transformExpr(e), pos)
    case Ord(e, pos)        => Ord(transformExpr(e), pos)
    case Chr(e, pos)        => Chr(transformExpr(e), pos)
    case BitwiseNot(e, pos) => BitwiseNot(transformExpr(e), pos)
    case Mul(l, r, pos)     => Mul(transformExpr(l), transformExpr(r), pos)
    case Div(l, r, pos)     => Div(transformExpr(l), transformExpr(r), pos)
    case Mod(l, r, pos)     => Mod(transformExpr(l), transformExpr(r), pos)
    case Plus(l, r, pos)    => Plus(transformExpr(l), transformExpr(r), pos)
    case Sub(l, r, pos)     => Sub(transformExpr(l), transformExpr(r), pos)
    case GT(l, r, pos)      => GT(transformExpr(l), transformExpr(r), pos)
    case GTE(l, r, pos)     => GTE(transformExpr(l), transformExpr(r), pos)
    case LT(l, r, pos)      => LT(transformExpr(l), transformExpr(r), pos)
    case LTE(l, r, pos)     => LTE(transformExpr(l), transformExpr(r), pos)
    case Equal(l, r, pos)   => Equal(transformExpr(l), transformExpr(r), pos)
    case NotEqual(l, r, pos) =>
      NotEqual(transformExpr(l), transformExpr(r), pos)
    case And(l, r, pos) => And(transformExpr(l), transformExpr(r), pos)
    case Or(l, r, pos)  => Or(transformExpr(l), transformExpr(r), pos)
    case BitwiseAnd(l, r, pos) =>
      BitwiseAnd(transformExpr(l), transformExpr(r), pos)
    case BitwiseOr(l, r, pos) =>
      BitwiseOr(transformExpr(l), transformExpr(r), pos)
    case BitwiseXor(l, r, pos) =>
      BitwiseXor(transformExpr(l), transformExpr(r), pos)
    case LogicalShiftLeft(l, r, pos) =>
      Plus(transformExpr(l), transformExpr(r), pos)
    case LogicalShiftRight(l, r, pos) =>
      Plus(transformExpr(l), transformExpr(r), pos)
    case id: Ident =>
      val id2 @ Ident(x, _) = updateIdent(id)
      kvs.getOrElse(x, id2)
    case ArrayElem(id, es, pos) =>
      ArrayElem(updateIdent(id), es.map(transformExpr), pos)
    case x: IntLiter  => x
    case b: BoolLiter => b
    case c: CharLiter => c
    case s: StrLiter  => s
    case p: PairLiter => p
  }

  private def deadCodeElimination(
      rhs: AssignRHS,
      stat: Stat,
      buf: ListBuffer[Stat]
  ): ListBuffer[Stat] =
    rhs match {
      case _: Expr => buf
      case _       => buf += stat
    }

  private def updateRhs(rhs: AssignRHS): AssignRHS = rhs match {
    case e: Expr => transformExpr(e)
    case _       => rhs
  }

  private def updateLhs(lhs: AssignLHS): AssignLHS = lhs match {
    case DerefPtr(ptr, pos)  => DerefPtr(transformExpr(ptr), pos)
    case Fst(id: Ident, pos) => Fst(updateIdent(id), pos)
    case Snd(id: Ident, pos) => Snd(updateIdent(id), pos)
    case ArrayElem(id, es, pos) =>
      ArrayElem(updateIdent(id), es.map(transformExpr), pos)
    // Semantically incorrect
    case _ => ???
  }

  /* Transforms a given statement S into SSA form. */
  private def transformStat(s: Stat): ListBuffer[Stat] = {
    val buf = ListBuffer.empty[Stat]
    s match {
      case EqIdent(t, id @ Ident(_, pos), r) =>
        val v = addToHashMap(id, r)
        deadCodeElimination(r, EqIdent(t, v, r), buf)
      case EqAssign(id: Ident, r) =>
        val v = addToHashMap(id, r)
        deadCodeElimination(r, EqAssign(v, r), buf)
      case EqAssign(lhs, rhs) => buf += EqAssign(updateLhs(lhs), updateRhs(rhs))
      case Read(id: Ident) =>
        val Ident(s, _) = updateIdent(id)
        val e = kvs(s)
        // add to dict but not kvs
        val v = addToHashMap(id, null)
        buf += EqIdent(getExprType(e), v, e)
        buf += Read(v)
      case Read(lhs)  => buf += Read(updateLhs(lhs))
      case Free(e)    => buf += Free(transformExpr(e))
      case Return(e)  => buf += Return(transformExpr(e))
      case Exit(e)    => buf += Exit(transformExpr(e))
      case Print(e)   => buf += Print(transformExpr(e))
      case PrintLn(e) => buf += PrintLn(transformExpr(e))
      case If(cond, s1, s2) =>
        val BoolLiter(b, _) = foldExpr(cond)
        if (b) buf ++= transformStat(s1) else buf ++= transformStat(s2)
      case Seq(statList) => statList.flatMap(transformStat).to(ListBuffer)
      case Skip          => buf += Skip
      case _             => ???
    }
  }

  /* Transforms a given function F into SSA form. */
  def transformFunc(f: Func): Func = {
    val Func(t, id, params, s) = f
    // TODO: transform params
    Func(t, id, params, Seq(transformStat(s).toList))
  }

  /* Transforms a given program AST into SSA form. */
  def toSSA(ast: Program): Program = {
    val Program(fs, s) = ast
    val x = Program(fs.map(f => transformFunc(f)), Seq(transformStat(s).toList))
    println(x)
    x
  }

}
