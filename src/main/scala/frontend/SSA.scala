package frontend

import Rules._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

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
    case Plus(l, r, pos) => Plus(transformExpr(l), transformExpr(r), pos)
    case id: Ident =>
      val Ident(x, pos) = updateIdent(id)
      kvs(x)
    case x: IntLiter  => x
    case b: BoolLiter => b
    case c: CharLiter => c
    case s: StrLiter  => s
    case p: PairLiter => p
  }

  private def foo(rhs: AssignRHS, stat: Stat): Stat = rhs match {
    case _: Expr => null
    case _       => stat
  }

  /* Transforms a given statement S into SSA form. */
  private def transformStat(s: Stat): Stat = {
    s match {
      case EqIdent(t, id @ Ident(_, pos), r) =>
        val v = addToHashMap(id, r)
        foo(r, EqIdent(t, v, r))
      case EqAssign(id: Ident, r) =>
        val v = addToHashMap(id, r)
        foo(r, EqAssign(v, r))
      case PrintLn(e) => PrintLn(transformExpr(e))
      case Seq(statList) =>
        Seq(statList.map(transformStat).filter(x => x != null))
      case _ => ???
    }
  }

  /* Transforms a given function F into SSA form. */
  def transformFunc(f: Func): Func = {
    val Func(t, id, params, s) = f
    // TODO: transform params
    Func(t, id, params, transformStat(s))
  }

  /* Transforms a given program AST into SSA form. */
  def toSSA(ast: Program): Program = {
    val Program(fs, s) = ast
    val x = Program(fs.map(f => transformFunc(f)), transformStat(s))
    println(x)
    x
  }

}
