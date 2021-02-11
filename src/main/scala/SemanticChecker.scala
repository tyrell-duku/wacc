import scala.collection.mutable.HashMap
import Rules._
import parsley.combinator.eof
import java.io.File
import Parser._

object SemanticChecker {
  private def convertToTable(f: Func): (Ident, Meta) = {
    val Func(t, i, ps, _) = f
    (i, Meta(t, ps))
  }

  def progAnalysis(p: Program): Unit = {
    val Program(funcs, s) = p
    val globalTable = SymbolTable(null, null)
    val globalFuncs = funcs.map(convertToTable)
    globalTable.dict.addAll(globalFuncs)

    for (f <- funcs) {
      funcAnalysis(f, SymbolTable(globalTable, f.id))
    }

    statAnalysis(s, globalTable)

  }

  def getParam(p: Param): (Ident, Type) = {
    val Param(t, i) = p
    (i, t)
  }

  def funcAnalysis(f: Func, sTable: SymbolTable): Unit = {
    val Func(_, _, ps, s) = f
    if (ps.isDefined) {
      val Some(ParamList(pList)) = ps
      sTable.addAll(pList.map(getParam))
    }
    statAnalysis(s, sTable)
  }

  def eqIdentAnalysis(
      lhsType: Type,
      id: Ident,
      aRHS: AssignRHS,
      sTable: SymbolTable
  ): Unit = {
    if (sTable.containScope(id)) {
      return println(
        "Variable " + id.s + " has already been declared within this scope, it cannot be redefined"
      )
    }
    val rhsType = aRHS.getType(sTable)
    sTable.add(id, lhsType)
    if (lhsType != rhsType) {
      return println(
        "Type mismatch, expected type: " + lhsType + ", actual type: " + rhsType
      )
    }
  }

  def eqAssignAnalysis(
      lhs: AssignLHS,
      rhs: AssignRHS,
      sTable: SymbolTable
  ): Unit = lhs match {
    case elem: PairElem =>
      eqAssignPairElem(elem, rhs, sTable)
    case Ident(s) =>
      eqAssignIdent(Ident(s), rhs, sTable)
    case ArrayElem(id, _) =>
      val lhsType = id.getType(sTable)
      if (lhsType == StringT) {
        return println("Element access is not permitted for strings")
      }
      eqAssignIdent(id, rhs, sTable)
  }

  def eqAssignIdent(id: Ident, rhs: AssignRHS, sTable: SymbolTable): Unit = {
    if (!sTable.contains(id)) {
      return println("Variable " + id.s + "  is undeclared in current scope")
    }
    val lhsType = id.getType(sTable)
    val rhsType = rhs.getType(sTable)
    if (lhsType != rhsType) {
      return println(
        "Type mismatch, expected type: " + lhsType + ", actual type: " + rhsType
      )
    }
  }

  def eqAssignPairElem(
      pe: PairElem,
      rhs: AssignRHS,
      sTable: SymbolTable
  ): Unit = pe match {
    case Fst(Ident(s)) =>
      val typeInFst = Ident(s).getType(sTable)
      val rhsType = rhs.getType(sTable)
      typeInFst match {
        case Pair(PairElemT(t), _) =>
          if (t != rhsType)
            println(
              "Type mismatch, expected type: " + t + ", actual type: " + rhsType
            )
        case Pair(PairElemPair, _) =>
          rhsType match {
            case Pair(_, _) =>
            case _ =>
              println(
                "Type mismatch, expected type: Pair" + ", actual type: " + rhsType
              )
          }
        case _ =>
          println(
            "Invalid type in fst, expected type: Pair" + ", actual type: " + typeInFst
          )
      }
    case Snd(Ident(s)) =>
      val typeInSnd = Ident(s).getType(sTable)
      val rhsType = rhs.getType(sTable)
      typeInSnd match {
        case Pair(PairElemT(t), _) =>
          if (t != rhsType)
            println(
              "Type mismatch, expected type: " + t + ", actual type: " + rhsType
            )
        case Pair(PairElemPair, _) =>
          rhsType match {
            case Pair(_, _) =>
            case _ =>
              println(
                "Type mismatch, expected type: Pair, actual type: " + rhsType
              )
          }
        case _ =>
          println(
            "Invalid type in snd, expected type: Pair, actual type: " + typeInSnd
          )
      }
    case _ => println("Invalid type within pair-elem, expected type: Pair")
  }

  def statAnalysis(s: Stat, sTable: SymbolTable): Unit = s match {
    case EqIdent(t, i, r) => eqIdentAnalysis(t, i, r, sTable)
    case EqAssign(l, r)   => eqAssignAnalysis(l, r, sTable)
    case Read(lhs) =>
      lhs match {
        case Ident(v) =>
          val lhsType = Ident(v).getType(sTable)
          lhsType match {
            case Pair(_, _) | ArrayT(_) =>
            case _ =>
              println(
                "Read statement expecting PairElem or Array Actual: " + lhsType
              )
          }
        case _ => println("Read statement expecting an Ident Actual: " + lhs)
      }
    case Free(e) =>
      e.getType(sTable) match {
        case Pair(_, _) | ArrayT(_) =>
        case _ =>
          println(
            "free statement expecting: Pair or Array, Acutal: " + e.getType(
              sTable
            )
          )
      }
    case Return(e) =>
      val t = e.getType(sTable)
      val ft = sTable.getFuncRetType
      if (t != ft) {
        if (ft == Err) {
          return println("Cannot return from main")
        }
        println(
          "Return type: " + t + " does not match function return type: " + ft
        )
      }
    case Exit(e) =>
      val t = e.getType(sTable)
      if (t != IntT) {
        println("exit statement expecting: Int, Actual: " + t)
      }
    case Print(e) =>
      val t = e.getType(sTable)
      if (t == Err) {
        println("Unable to resolve expression " + t)
      }
    case PrintLn(e) =>
      val t = e.getType(sTable)
      if (t == Err) {
        println("Unable to resolve expression " + t)
      }
    case If(cond, s1, s2) =>
      val condType = cond.getType(sTable)
      if (condType != BoolT) {
        println("If condition expecting: Bool, Actual: " + condType)
      }
      val ifScope = SymbolTable(sTable, sTable.funcId)
      val elseScope = SymbolTable(sTable, sTable.funcId)
      statAnalysis(s1, ifScope)
      statAnalysis(s2, elseScope)
    case While(cond, s) =>
      val condType = cond.getType(sTable)
      if (condType != BoolT) {
        println(
          "While condition expecting: Bool, Actual: " + condType
        )
      }
      val whileScope = SymbolTable(sTable, sTable.funcId)
      statAnalysis(s, whileScope)
    case Begin(s) =>
      val beginScope = SymbolTable(sTable, sTable.funcId)
      statAnalysis(s, beginScope)
    case Seq(x) => x.map(s => statAnalysis(s, sTable))
    case _      => // ignore Skip
  }
}
