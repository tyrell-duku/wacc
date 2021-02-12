import Rules._

class SemanticChecker {
  private var semErrors: List[SemanticError] = List.empty[SemanticError]

  private def funcToSTable(f: Func): (Ident, Meta) = {
    val Func(typeOf, id, params, _) = f
    val ParamList(pList) = params.getOrElse(ParamList(List()))
    val paramTypes = pList.map(_.t)
    (id, Meta(typeOf, Some(paramTypes)))
  }

  def progAnalysis(p: Program): List[SemanticError] = {
    val Program(funcs, stat) = p
    val globalTable = SymbolTable(null, null)
    val globalFuncs = funcs.map(funcToSTable)
    globalTable.addAll(globalFuncs)

    for (f <- funcs) {
      funcAnalysis(f, SymbolTable(globalTable, f.id))
    }

    statAnalysis(stat, globalTable)

    semErrors.reverse
  }

  def funcAnalysis(f: Func, sTable: SymbolTable): Unit = {
    val Func(_, _, params, s) = f
    if (params.isDefined) {
      val Some(ParamList(pList)) = params
      val sTableData = pList.map((p: Param) => (p.id, Meta(p.t, None)))
      sTable.addAll(sTableData)
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
      semErrors ::= variableDeclared(id)
      return
    }
    val rhsType = rhs.getType(sTable)
    if (rhs.semErrs.nonEmpty) {
      semErrors :::= rhs.semErrs
    }
    sTable.add(id, lhsType)
    if (lhsType != rhsType) {
      semErrors ::= typeMismatch(rhs, rhsType, List(lhsType))
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
        semErrors :::= id.semErrs
      }
      lhsType match {
        case ArrayT(_) =>
        case _ =>
          semErrors ::= elementAccessDenied(id)
          return
      }
      val ArrayT(t) = lhsType
      val rT = rhs.getType(sTable)
      if (rhs.semErrs.nonEmpty) {
        semErrors :::= rhs.semErrs
      }
      if (rT != t) {
        semErrors ::= typeMismatch(rhs, rT, List(t))
      }
  }

  def eqAssignIdent(id: Ident, rhs: AssignRHS, sTable: SymbolTable): Unit = {
    if (!sTable.contains(id)) {
      semErrors ::= variableNotDeclared(id)
      return
    }
    if (sTable.isFunc(id)) {
      semErrors ::= functionIllegalAssignment(id)
      return
    }
    val lhsType = id.getType(sTable)
    if (id.semErrs.nonEmpty) {
      semErrors :::= id.semErrs
    }
    val rhsType = rhs.getType(sTable)
    if (rhs.semErrs.nonEmpty) {
      semErrors :::= rhs.semErrs
    }
    // && !rhsType.isErr
    if (lhsType != rhsType) {
      semErrors ::= typeMismatch(rhs, rhsType, List(lhsType))
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
            semErrors ::= typeMismatch(rhs, rhsType, List(t))
        case Pair(PairElemPair, _) =>
          rhsType match {
            case Pair(_, _) =>
            case _ =>
              semErrors ::= typeMismatch(rhs, rhsType, List(Pair(null, null)))
          }
        case _ => semErrors ::= typeMismatch(rhs, rhsType, List(Pair(null, null)))
      }
    case Snd(Ident(s)) =>
      val typeInSnd = Ident(s).getType(sTable)
      val rhsType = rhs.getType(sTable)
      typeInSnd match {
        case Pair(PairElemT(t), _) =>
          //&& !rhsType.isErr
          if (t != rhsType)
            semErrors ::= typeMismatch(rhs, rhsType, List(t))
        case Pair(PairElemPair, _) =>
          rhsType match {
            case Pair(_, _) =>
            case _ =>
              semErrors ::= typeMismatch(rhs, rhsType, List(Pair(null, null)))
          }
        case _ =>
          semErrors ::= typeMismatch(rhs, typeInSnd, List(Pair(null, null)))
      }
    case _ =>
      semErrors ::= typeMismatch(rhs, rhs.getType(sTable), List(Pair(null, null)))
  }

  def statAnalysis(s: Stat, sTable: SymbolTable): Unit = s match {
    case EqIdent(t, i, r) => eqIdentAnalysis(t, i, r, sTable)
    case EqAssign(l, r)   => eqAssignAnalysis(l, r, sTable)
    case Read(lhs) =>
      lhs match {
        case elem: Ident =>
          val t = elem.getType(sTable)
          if (elem.semErrs.nonEmpty) {
            semErrors :::= elem.semErrs
          }
          t match {
            case CharT | IntT =>
            case _ =>
              semErrors ::= typeMismatch(elem, t, List(CharT, IntT))
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
}
