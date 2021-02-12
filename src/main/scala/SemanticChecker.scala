import Rules._

class SemanticChecker {
  private var semErrors: List[SemanticError] = List.empty[SemanticError]

  // Translates a function F into a symbol table entry
  private def funcToSTable(f: Func): (Ident, Meta) = {
    val Func(typeOf, id, params, _) = f
    val ParamList(pList) = params.getOrElse(ParamList(List()))
    val paramTypes = pList.map(_.t)
    (id, Meta(typeOf, Some(paramTypes)))
  }

  // Performs analysis on program P, returning a list of all semantic errors
  // that have occured (if any).
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

  // Analyses a single function F by adding its parameters to STABLE
  def funcAnalysis(f: Func, sTable: SymbolTable): Unit = {
    val Func(_, _, params, s) = f
    params match {
      case Some(ParamList(pList)) =>
        sTable.addAll(pList.map((p: Param) => (p.id, Meta(p.t, None))))
      case _ =>
    }
    statAnalysis(s, sTable)
  }

  // Analyses the statement LHSTYPE ID = RHS
  def eqIdentAnalysis(
      lhsType: Type,
      id: Ident,
      rhs: AssignRHS,
      sTable: SymbolTable
  ): Unit = {
    if (sTable.contains(id)) {
      // Error case: redeclaration of variable not allowed
      semErrors ::= variableDeclared(id)
      return
    }
    // Checks for semantics errors for RHS
    val rhsType = rhs.getType(sTable)
    if (rhs.semErrs.nonEmpty) {
      semErrors :::= rhs.semErrs
      sTable.add(id, lhsType)
      return
    }
    sTable.add(id, lhsType)
    if (lhsType != rhsType) {
      // Error case: type mismatch between LHS and RHS
      semErrors ::= typeMismatch(rhs, rhsType, List(lhsType))
    }
  }

  // Analyses the statement ASSIGNLHS = ASSIGNRHS
  def eqAssignAnalysis(
      lhs: AssignLHS,
      rhs: AssignRHS,
      sTable: SymbolTable
  ): Unit = lhs match {
    case elem: PairElem =>
      eqAssignPairElem(elem, rhs, sTable)
    case i @ Ident(s) =>
      eqAssignIdent(i, rhs, sTable)
    case ArrayElem(id, e) =>
      // Checks for any semantic errors from id of array
      val lhsType = id.getType(sTable)
      if (id.semErrs.nonEmpty) {
        semErrors :::= id.semErrs
      }
      // Ensuring all indexes are ints
      for (index <- e) {
        val indexT = index.getType(sTable)
        if (indexT != IntT) {
          semErrors ::= typeMismatch(index, indexT, List(IntT))
        }
      }
      lhsType match {
        case ArrayT(_) =>
        // Error case: attempt to index a non-indxable type (e.g. strings)
        case _ =>
          semErrors ::= elementAccessDenied(id)
          return
      }
      val ArrayT(t) = lhsType
      val rT = rhs.getType(sTable)
      if (rhs.semErrs.nonEmpty) {
        semErrors :::= rhs.semErrs
        return
      }
      if (rT != t) {
        semErrors ::= typeMismatch(rhs, rT, List(t))
      }

  }

  // Analyses the statement ID = RHS
  def eqAssignIdent(id: Ident, rhs: AssignRHS, sTable: SymbolTable): Unit = {
    // Error case: identifier ID is used without being declared - (not found in) symbol table
    if (!sTable.contains(id)) {
      semErrors ::= variableNotDeclared(id)
      return
    }
    // Error case: identifier ID is a declared function
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
      return
    }
    if (lhsType != rhsType) {
      // Error case: LHS and RHS have different types
      semErrors ::= typeMismatch(rhs, rhsType, List(lhsType))
    }
  }

  def eqAssignPairElem(
      pe: PairElem,
      rhs: AssignRHS,
      sTable: SymbolTable
  ): Unit = {
    val lhsType = pe.getType(sTable)
    val rhsType = rhs.getType(sTable)
    if (pe.semErrs.nonEmpty) {
      semErrors :::= pe.semErrs
      return
    }
    if (rhs.semErrs.nonEmpty) {
      semErrors :::= rhs.semErrs
      return
    }
    if (lhsType != rhsType) {
      semErrors ::= typeMismatch(rhs, rhsType, List(lhsType))
    }
  }

  def readAnalysis(elem: AssignRHS, sTable: SymbolTable) {
    val t = elem.getType(sTable)
    if (elem.semErrs.nonEmpty) {
      semErrors :::= elem.semErrs
    }
    t match {
      case CharT | IntT =>
      case _ =>
        semErrors ::= typeMismatch(elem, t, List(CharT, IntT))
    }
  }

  def statAnalysis(s: Stat, sTable: SymbolTable): Unit = s match {
    case EqIdent(t, i, r) => eqIdentAnalysis(t, i, r, sTable)
    case EqAssign(l, r)   => eqAssignAnalysis(l, r, sTable)
    case Read(lhs) =>
      lhs match {
        case elem: Ident     => readAnalysis(elem, sTable)
        case elem: ArrayElem => readAnalysis(elem, sTable)
        case elem: PairElem  => readAnalysis(elem, sTable)
      }
    case Free(e) =>
      val eType = e.getType(sTable)
      eType match {
        case Pair(_, _) | ArrayT(_) =>
        case _ =>
          semErrors ::= typeMismatch(
            e,
            eType,
            List(Pair(null, null), ArrayT(null))
          )
      }
    case Return(e) =>
      val returnType = sTable.getFuncRetType
      // null if in global scope or unable to return
      if (returnType == null) {
        semErrors ::= invalidReturn(e)
        return
      }
      // getType generates semantic errors for E
      val t = e.getType(sTable)
      if (e.semErrs.nonEmpty) {
        semErrors :::= e.semErrs
        return
      }
      // Error case:
      if (t != returnType) {
        semErrors ::= typeMismatch(e, t, List(returnType))
      }
    case Exit(e) =>
      val t = e.getType(sTable)
      if (e.semErrs.nonEmpty) {
        semErrors :::= e.semErrs
        return
      }
      if (t != IntT) {
        semErrors ::= typeMismatch(e, t, List(IntT))
      }
    case Print(e) =>
      e.getType(sTable)
      if (e.semErrs.nonEmpty) {
        semErrors :::= e.semErrs
      }
    case PrintLn(e) =>
      e.getType(sTable)
      if (e.semErrs.nonEmpty) {
        semErrors :::= e.semErrs
      }
    case If(cond, s1, s2) =>
      val condType = cond.getType(sTable)
      if (cond.semErrs.nonEmpty) {
        semErrors :::= cond.semErrs
      } else {
        if (condType != BoolT) {
          semErrors ::= typeMismatch(cond, condType, List(BoolT))
        }
      }
      val ifScope = SymbolTable(sTable, sTable.funcId)
      val elseScope = SymbolTable(sTable, sTable.funcId)
      statAnalysis(s1, ifScope)
      statAnalysis(s2, elseScope)
    case While(cond, s) =>
      val condType = cond.getType(sTable)
      if (cond.semErrs.nonEmpty) {
        semErrors :::= cond.semErrs
      } else {
        if (condType != BoolT) {
          semErrors ::= typeMismatch(cond, condType, List(BoolT))
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
