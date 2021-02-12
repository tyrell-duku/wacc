import Rules._
import parsley.combinator.eof
import java.io.File
import Parser._
import parsley.Parsley

class SemanticChecker {
  private var semErrs: List[SemanticError] = List()

  private def convertToTable(f: Func): (Ident, Meta) = {
    val Func(t, i, ps, _) = f
    val ParamList(fromOption) = ps.getOrElse(ParamList(List()))
    val res = fromOption.map((p: Param) => p.t)
    (i, Meta(t, Some(res)))
  }

  def progAnalysis(p: Program): List[SemanticError] = {
    val Program(funcs, s) = p
    val globalTable = SymbolTable(null, null)
    val globalFuncs = funcs.map(convertToTable)
    globalTable.addAll(globalFuncs)

    for (f <- funcs) {
      funcAnalysis(f, SymbolTable(globalTable, f.id))
    }

    statAnalysis(s, globalTable)

    semErrs.reverse
  }

  def getParam(p: Param): (Ident, Type) = {
    val Param(t, i) = p
    (i, t)
  }

  def funcAnalysis(f: Func, sTable: SymbolTable): Unit = {
    val Func(_, _, ps, s) = f
    if (ps.isDefined) {
      val Some(ParamList(pList)) = ps
      val toMeta =
        pList.map(getParam).map((x: (Ident, Type)) => (x._1, Meta(x._2, None)))
      sTable.addAll(toMeta)
    }
    statAnalysis(s, sTable)
  }

  def eqIdentAnalysis(
      lhsType: Type,
      id: Ident,
      rhs: AssignRHS,
      sTable: SymbolTable
  ): Unit = {
    if (sTable.contains(id)) {
      semErrs ::= variableDeclared(id)
      return
    }
    val rhsType = rhs.getType(sTable)
    if (rhs.semErrs.nonEmpty) {
      semErrs :::= rhs.semErrs
      return
    }
    rhs match {
      case Call(id: Ident, args: Option[ArgList]) =>
        val paramCheck = sTable.funcParamMatch(id, args)
        semErrs :::= paramCheck
      case _ =>
    }
    sTable.add(id, lhsType)
    if (lhsType != rhsType) {
      semErrs ::= typeMismatch(rhs, rhsType, List(lhsType))
    }
  }

  def eqAssignAnalysis(
      lhs: AssignLHS,
      rhs: AssignRHS,
      sTable: SymbolTable
  ): Unit = lhs match {
    case elem: PairElem =>
      eqAssignPairElem(elem, rhs, sTable)
    case i @ Ident(s) =>
      eqAssignIdent(i, rhs, sTable)
    case ArrayElem(id, _) =>
      val lhsType = id.getType(sTable)
      if (id.semErrs.nonEmpty) {
        semErrs :::= id.semErrs
      }
      lhsType match {
        case ArrayT(_) =>
        case _ =>
          semErrs ::= elementAccessDenied(id)
          return
      }
      val ArrayT(t) = lhsType
      val rT = rhs.getType(sTable)
      if (rhs.semErrs.nonEmpty) {
        semErrs :::= rhs.semErrs
      }
      if (rT != t) {
        semErrs ::= typeMismatch(rhs, rT, List(t))
      }
  }

  def eqAssignIdent(id: Ident, rhs: AssignRHS, sTable: SymbolTable): Unit = {
    if (!sTable.contains(id)) {
      semErrs ::= variableNotDeclared(id)
      return
    }
    if (sTable.isFunc(id)) {
      semErrs ::= functionIllegalAssignment(id)
      return
    }
    val lhsType = id.getType(sTable)
    if (id.semErrs.nonEmpty) {
      semErrs :::= id.semErrs
    }
    val rhsType = rhs.getType(sTable)
    if (rhs.semErrs.nonEmpty) {
      semErrs :::= rhs.semErrs
    }
    // && !rhsType.isErr
    if (lhsType != rhsType) {
      semErrs ::= typeMismatch(rhs, rhsType, List(lhsType))
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
          //&& !rhsType.isErr
          if (t != rhsType)
            semErrs ::= typeMismatch(rhs, rhsType, List(t))
        case Pair(PairElemPair, _) =>
          rhsType match {
            case Pair(_, _) =>
            case _ =>
              semErrs ::= typeMismatch(rhs, rhsType, List(Pair(null, null)))
          }
        case _ => semErrs ::= typeMismatch(rhs, rhsType, List(Pair(null, null)))
      }
    case Snd(Ident(s)) =>
      val typeInSnd = Ident(s).getType(sTable)
      val rhsType = rhs.getType(sTable)
      typeInSnd match {
        case Pair(PairElemT(t), _) =>
          //&& !rhsType.isErr
          if (t != rhsType)
            semErrs ::= typeMismatch(rhs, rhsType, List(t))
        case Pair(PairElemPair, _) =>
          rhsType match {
            case Pair(_, _) =>
            case _ =>
              semErrs ::= typeMismatch(rhs, rhsType, List(Pair(null, null)))
          }
        case _ =>
          semErrs ::= typeMismatch(rhs, typeInSnd, List(Pair(null, null)))
      }
    case _ =>
      semErrs ::= typeMismatch(rhs, rhs.getType(sTable), List(Pair(null, null)))
  }

  def statAnalysis(s: Stat, sTable: SymbolTable): Unit = s match {
    case EqIdent(t, i, r) => eqIdentAnalysis(t, i, r, sTable)
    case EqAssign(l, r)   => eqAssignAnalysis(l, r, sTable)
    case Read(lhs) =>
      lhs match {
        case elem: Ident =>
          val t = elem.getType(sTable)
          if (elem.semErrs.nonEmpty) {
            semErrs :::= elem.semErrs
          }
          t match {
            case CharT | IntT =>
            case _ =>
              semErrs ::= typeMismatch(elem, t, List(CharT, IntT))
          }
        case elem: ArrayElem =>
          val t = elem.getType(sTable)
          if (elem.semErrs.nonEmpty) {
            semErrs :::= elem.semErrs
          }
          t match {
            case CharT | IntT =>
            case _ =>
              semErrs ::= typeMismatch(elem, t, List(CharT, IntT))
          }
        case elem: PairElem =>
          val t = elem.getType(sTable)
          if (elem.semErrs.nonEmpty) {
            semErrs :::= elem.semErrs
          }
          t match {
            case CharT | IntT =>
            case _ =>
              semErrs ::= typeMismatch(elem, t, List(CharT, IntT))
          }
      }
    case Free(e) =>
      val eType = e.getType(sTable)
      eType match {
        case Pair(_, _) | ArrayT(_) =>
        case _ =>
          semErrs ::= typeMismatch(
            e,
            eType,
            List(Pair(null, null), ArrayT(null))
          )
      }
    case Return(e) =>
      val returnType = sTable.getFuncRetType
      if (returnType == null) {
        semErrs ::= invalidReturn(e)
        return
      }
      val t = e.getType(sTable)
      if (e.semErrs.nonEmpty) {
        semErrs :::= e.semErrs
        return
      }
      if (t != returnType) {
        semErrs ::= typeMismatch(e, t, List(returnType))
      }

    case Exit(e) =>
      val t = e.getType(sTable)
      if (e.semErrs.nonEmpty) {
        semErrs :::= e.semErrs
        return
      }
      if (t != IntT) {
        semErrs ::= typeMismatch(e, t, List(IntT))
      }
    case Print(e) =>
      e.getType(sTable)
      if (e.semErrs.nonEmpty) {
        semErrs :::= e.semErrs
      }
    case PrintLn(e) =>
      e.getType(sTable)
      if (e.semErrs.nonEmpty) {
        semErrs :::= e.semErrs
      }
    case If(cond, s1, s2) =>
      val condType = cond.getType(sTable)
      if (cond.semErrs.nonEmpty) {
        semErrs :::= cond.semErrs
      } else {
        if (condType != BoolT) {
          semErrs ::= typeMismatch(cond, condType, List(BoolT))
        }
      }
      val ifScope = SymbolTable(sTable, sTable.funcId)
      val elseScope = SymbolTable(sTable, sTable.funcId)
      statAnalysis(s1, ifScope)
      statAnalysis(s2, elseScope)
    case While(cond, s) =>
      val condType = cond.getType(sTable)
      if (cond.semErrs.nonEmpty) {
        semErrs :::= cond.semErrs
      } else {
        if (condType != BoolT) {
          semErrs ::= typeMismatch(cond, condType, List(BoolT))
        }
      }
      val whileScope = SymbolTable(sTable, sTable.funcId)
      statAnalysis(s, whileScope)
    case Begin(s) =>
      val beginScope = SymbolTable(sTable, sTable.funcId)
      statAnalysis(s, beginScope)
    case Seq(x) => x.foreach(s => statAnalysis(s, sTable))
    case _      => // ignore Skip
  }

  def getAllSemErrors(): Unit ={

  }
}
