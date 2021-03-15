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
      case arr: ArrayLiter =>
        arr match {
          case ArrayLiter(Some(es), _) =>
            for (i <- es.indices) {
              val aeStr = str + "-" + i
              dict += ((aeStr, (x, y)))
              kvs += ((x.toString + aeStr, transformExpr(es(i))))
            }
          case _ =>
        }
      case _ =>
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

  /* Transforms an expression E. */
  private def transformExpr(e: Expr): Expr = e match {
    case sizeof: SizeOf => sizeof
    case id: Ident =>
      val id2 @ Ident(x, _) = updateIdent(id)
      kvs.getOrElse(x, id2)
    case ArrayElem(id @ Ident(s, _), es, pos) =>
      val arrayElemName = arrayElemIdentifier(s, es)
      val (_, y) = dict(arrayElemName)
      kvs.getOrElse(
        y.toString + arrayElemName,
        Ident(y.toString + arrayElemName, null)
      )
    case x: IntLiter  => x
    case b: BoolLiter => b
    case c: CharLiter => c
    case s: StrLiter  => s
    case p: PairLiter => p
    // All operators
    case _ => e.map(transformExpr)
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

  private def arrayElemIdentifier(varName: String, es: List[Expr]): String = {
    varName + "-" + es.map(transformExpr).mkString("-")
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
      case EqAssign(ArrayElem(id @ Ident(s, idPos), es, pos), r) =>
        addToHashMap(Ident(arrayElemIdentifier(s, es), idPos), r)
        buf
      case EqAssign(lhs, rhs) => buf += EqAssign(updateLhs(lhs), updateRhs(rhs))
      case Read(id: Ident) =>
        val Ident(s, _) = updateIdent(id)
        val e = kvs(s)
        // add to dict but not kvs
        val v = addToHashMap(id, null)
        buf += EqIdent(getExprType(e), v, e)
        buf += Read(v)
      case Read(ArrayElem(id @ Ident(s, _), es, pos)) =>
        val arrayElemName = arrayElemIdentifier(s, es)
        val (_, y) = dict(arrayElemName)
        val e = kvs(y.toString + arrayElemName)
        val v = addToHashMap(Ident(arrayElemName, null), null)
        buf += EqIdent(getExprType(e), v, e)
        buf += Read(v)
      case Read(lhs)  => buf += Read(updateLhs(lhs))
      case Free(e)    => buf += Free(transformExpr(e))
      case Return(e)  => buf += Return(transformExpr(e))
      case Exit(e)    => buf += Exit(transformExpr(e))
      case Print(e)   => buf += Print(transformExpr(e))
      case PrintLn(e) => buf += PrintLn(transformExpr(e))
      case If(cond, s1, s2) =>
        val BoolLiter(b, _) = foldExpr(transformExpr(cond))
        if (b) buf ++= transformStat(s1) else buf ++= transformStat(s2)
      case Seq(statList) => statList.flatMap(transformStat).to(ListBuffer)
      case Skip          => buf += Skip
      case _             => buf
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
    Program(fs.map(f => transformFunc(f)), Seq(transformStat(s).toList))
  }

}
