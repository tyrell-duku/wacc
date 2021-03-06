package frontend.Semantics

import frontend.Rules._

import scala.collection.mutable

class SemanticChecker {
  private var semErrors = mutable.ListBuffer.empty[SemanticError]

  // Evaluate the type of rhs, appending to semErrors if semantic errors
  // occur whilst checking typ
  private def checkType(rhs: AssignRHS, sTable: SymbolTable): Type = {
    val actualType = rhs.getType(sTable)
    if (rhs.semErrs.nonEmpty) {
      semErrors ++= rhs.semErrs
      return null
    }
    actualType
  }

  // Translates a function F into a symbol table entry
  private def funcToSTable(f: Func): (Ident, Meta) = {
    val Func(typeOf, id, params, _) = f
    val ParamList(pList) = params.getOrElse(ParamList(List()))
    val paramTypes = pList.map(_.t)
    (id, Meta(typeOf, Some(paramTypes)))
  }

  // Performs analysis on program P, returning a list of all semantic errors
  // that have occured (if any).
  def progAnalysis(
      p: Program
  ): (SymbolTable, mutable.ListBuffer[SemanticError]) = {
    val Program(funcs, stat) = p
    val globalTable: SymbolTable =
      SymbolTable(null, null, new mutable.HashMap[Ident, Meta])
    val globalFuncs = funcs.map(funcToSTable)
    semErrors ++= globalTable.addFuncs(globalFuncs)

    for (f <- funcs) {
      funcAnalysis(f, globalTable.nextScope(f.id))
    }

    statAnalysis(stat, globalTable)

    (globalTable, semErrors)
  }

  // Analyses a single function F by adding its parameters to STABLE
  def funcAnalysis(f: Func, sTable: SymbolTable): Unit = {
    val Func(_, _, params, s) = f
    params match {
      case Some(ParamList(pList)) =>
        sTable.addVars(pList.map((p: Param) => (p.id, p.t)))
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
    if (sTable.containScope(id)) {
      // Error case: redeclaration of variable not allowed
      semErrors += VariableDeclared(id)
      return
    }
    // Checks for semantics errors for RHS
    val rhsType = rhs.getType(sTable)
    if (rhs.semErrs.nonEmpty) {
      semErrors ++= rhs.semErrs
      sTable.add(id, lhsType)
      return
    }
    sTable.add(id, lhsType)
    if (lhsType != rhsType) {
      // Error case: type mismatch between LHS and RHS
      semErrors += TypeMismatch(rhs, rhsType, List(lhsType))
    }
  }

  // Analyses the statement ASSIGNLHS = ASSIGNRHS
  def eqAssignAnalysis(
      lhs: AssignLHS,
      rhs: AssignRHS,
      sTable: SymbolTable
  ): Unit = lhs match {
    case elem: PairElem =>
      checkAssignmentType(checkType(elem, sTable), rhs, sTable)
    case ident: Ident =>
      eqAssignIdent(ident, rhs, sTable)
    case arrayElem: ArrayElem =>
      checkAssignmentType(checkType(arrayElem, sTable), rhs, sTable)
    case ptr: DerefPtr =>
      checkAssignmentType(checkType(ptr, sTable), rhs, sTable)
  }

  // Analyses the statement ID = RHS
  def eqAssignIdent(id: Ident, rhs: AssignRHS, sTable: SymbolTable): Unit = {
    // Error case: identifier ID is used without being declared - (not found in) symbol table
    if (!sTable.contains(id)) {
      semErrors += VariableNotDeclared(id)
      return
    }
    // Error case: identifier ID is a declared function
    if (sTable.isFunc(id)) {
      semErrors += FunctionIllegalAssignment(id)
      return
    }
    checkAssignmentType(checkType(id, sTable), rhs, sTable)
  }

  private def checkAssignmentType(
      lhsType: Type,
      rhs: AssignRHS,
      sTable: SymbolTable
  ) = {
    val rhsType = checkType(rhs, sTable)
    // Add semantic error if types mismatch and both are invalid once evaluated
    if ((lhsType != null) && (rhsType != null) && (lhsType != rhsType)) {
      semErrors += TypeMismatch(rhs, rhsType, List(lhsType))
    }
  }

  // Analyses a statement of the form read <assign-rhs>
  def readAnalysis(elem: AssignRHS, sTable: SymbolTable): Unit = {
    val rhsType = checkType(elem, sTable)
    rhsType match {
      case CharT | IntT =>
      case _ =>
        semErrors += TypeMismatch(elem, rhsType, List(CharT, IntT))
    }
  }

  private def readStat(lhs: AssignLHS, sTable: SymbolTable) = {
    lhs match {
      case elem: Ident     => readAnalysis(elem, sTable)
      case elem: ArrayElem => readAnalysis(elem, sTable)
      case elem: PairElem  => readAnalysis(elem, sTable)
      case elem: DerefPtr  => readAnalysis(elem, sTable)
    }
  }

  private def freeStat(e: Expr, sTable: SymbolTable): Unit = {
    val t = checkType(e, sTable)
    if ((t == null) || t.isPair || t.isArray || t.isPtr) {
      e match {
        case _: Ident =>
        case _ =>
          semErrors += InvalidFree(e)
      }
    } else {
      // Error case: calling free only permitted for pairs/arrays/pointers
      semErrors += TypeMismatch(
        e,
        t,
        List(Pair(null, null), ArrayT(null), PtrT(null))
      )
    }
  }

  private def returnStat(e: Expr, sTable: SymbolTable): Unit = {
    val expectedType = sTable.getFuncRetType
    // null if in global scope or unable to return
    if (expectedType == null) {
      semErrors += InvalidReturn(e)
      return
    }
    // getType generates semantic errors for E
    val actualType = checkType(e, sTable)

    // Error case: type T doesnt match funcion return type RETURNTYPE
    if (actualType != expectedType) {
      semErrors += TypeMismatch(e, actualType, List(expectedType))
    }
  }

  private def exitStat(e: Expr, sTable: SymbolTable) = {
    val actualType = checkType(e, sTable)
    // Error case: attempt to exit with non-integer code
    if ((actualType != null) && (actualType != IntT)) {
      semErrors += TypeMismatch(e, actualType, List(IntT))
    }
  }

  private def condStat(cond: Expr, stat: List[Stat], sTable: SymbolTable) = {
    val condType = checkType(cond, sTable)
    if ((condType != null) && (condType != BoolT)) {
      // Error case: Condition COND for IF statement is not of type Boolean
      semErrors += TypeMismatch(cond, condType, List(BoolT))
    }
    // New symbol tables for conditional-statements, with independent analysis of each branch
    stat.foreach(s => statAnalysis(s, sTable.nextScope))
  }

  // Analyses a single statement
  def statAnalysis(s: Stat, sTable: SymbolTable): Unit = s match {
    case EqIdent(t, i, r) => eqIdentAnalysis(t, i, r, sTable)
    case EqAssign(l, r)   => eqAssignAnalysis(l, r, sTable)
    case Read(lhs)        => readStat(lhs, sTable)
    case Free(e)          => freeStat(e, sTable)
    case Return(e)        => returnStat(e, sTable)
    case Exit(e)          => exitStat(e, sTable)
    case Print(e)         => checkType(e, sTable)
    case PrintLn(e)       => checkType(e, sTable)
    case If(cond, s1, s2) => condStat(cond, List(s1, s2), sTable)
    case While(cond, s)   => condStat(cond, List(s), sTable)
    case Begin(s)         => statAnalysis(s, sTable.nextScope)
    case Seq(statList)    => statList.foreach(s => statAnalysis(s, sTable))
    case _                => // ignore Skip
  }
}
