import scala.collection.mutable.HashMap
import Rules._

object SemanticChecker {
  var funcTable = new HashMap[Ident, Type]

  var identTable = new HashMap[Ident, Type]

  private def convertToTable(f: Func): (Ident, Meta) = {
    val Func(t, i, ps, _) = f
    (i, Meta(t, ps))
  }

  def progAnalysis(p: Program): Unit = {
    val Program(funcs, s) = p
    val globalTable = SymbolTable(null, new HashMap[Ident, Meta])
    val globalFuncs = funcs.map(convertToTable)
    globalTable.dict.addAll(globalFuncs)

    for (f <- funcs) {
      funcAnalysis(f, SymbolTable(globalTable, new HashMap[Ident, Meta]))
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
    if (lhsType != rhsType) {
      return println(
        "Type mismatch, expected type: " + lhsType + ", actual type: " + rhsType
      )
    }
    identTable.addOne(id, lhsType)
  }

  def eqAssignAnalysis(
      lhs: AssignLHS,
      rhs: AssignRHS,
      sTable: SymbolTable
  ): Unit = lhs match {
    case elem: PairElem   => eqAssignPairElem(elem, rhs, sTable)
    case Ident(s)         => eqAssignIdent(Ident(s), rhs, sTable)
    case ArrayElem(id, _) => eqAssignIdent(id, rhs, sTable)
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
                "Type mismatch, expected type: Pair" + ", actual type: " + rhsType
              )
          }
        case _ =>
          println(
            "Invalid type in snd, expected type: Pair" + ", actual type: " + typeInSnd
          )
      }
    case _ => println("Invalid type within pair-elem, expected type: Pair")
  }

  def statAnalysis(s: Stat, sTable: SymbolTable): Unit = s match {
    case EqIdent(t, i, r) => eqIdentAnalysis(t, i, r, sTable)
    case EqAssign(l, r)   => eqAssignAnalysis(l, r, sTable)
    case Read(x) =>
      x match {
        case Ident(v) =>
          Ident(v).getType(sTable) match {
            case Pair(_, _) | ArrayT(_) =>
            case _                      => println("ERROR")
          }
        case _ => println("ERROR")
      }
    case Free(x) =>
      x.getType(sTable) match {
        case Pair(_, _) | ArrayT(_) =>
        case _                      => println("ERROR")
      }
    // case Return(x)      => // TODO: type check x ?
    case Exit(x)    => if (x.getType(sTable) != IntT) { println("ERROR") }
    case Print(x)   => if (x.getType(sTable) == Err) { println("ERROR") }
    case PrintLn(x) => if (x.getType(sTable) == Err) { println("ERROR") }
    case If(x, s1, s2) =>
      if (x.getType(sTable) != BoolT) { println("ERROR") }
    case While(x, s) => if (x.getType(sTable) != BoolT) { println("ERROR") }
    case Seq(x)      => x.map(s => statAnalysis(s, sTable))
    case _           => // ignore Skip
  }

}
