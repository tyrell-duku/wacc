package frontend

import Rules._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import backend.CodeGenerator.{getExprType, getBaseTypeSize}
import frontend.Semantics.SymbolTable
import frontend.ConstantFolding.fold

case class SSA(sTable: SymbolTable) {
  // Stack depth needed to store all variables in scope
  type VarStackSize = Int
  /* Type aliases */
  type VarName = String
  type UniqueNum = Int
  type CurrentAssignmentNum = Int
  type NumOfAssigns = Int
  type IdNums = (UniqueNum, CurrentAssignmentNum, NumOfAssigns)
  // hash map for unique identifying
  private var dict =
    new HashMap[VarName, IdNums]
  // (key, values) hash map for constant propagation
  private var kvs = new HashMap[VarName, AssignRHS]
  // Declared HeapVariables
  private var declaredHeapVars = ListBuffer.empty[Ident]
  private var currSTable = sTable
  /* Constants */
  val Initial_Id_Num = 0
  val Undefined_Value = null
  val Undefined_Pos = (-1, -1)
  val Undefined_Map = null
  val Dummy_Pos = (0, 0)
  val Is_Fst = true
  val Not_Fst = false
  /* Variables */
  var stackSize = 0

  /* Adds the NewPair NEWPAIR to the key value hash map. */
  private def addNewPairToHashMap(
      newpair: Newpair,
      originalStr: VarName
  ): VarName = {
    val uniqueId = addToDict(originalStr)
    val Newpair(fst, snd, _) = newpair
    println(s"newpair $newpair")
    // Fst elem
    val fstName = addToDict(pairElemIdentifier(originalStr, Is_Fst))
    kvs += ((fstName, transformExpr(fst)))
    // Snd elem
    val sndName = addToDict(pairElemIdentifier(originalStr, Not_Fst))
    kvs += ((sndName, transformExpr(snd)))
    kvs += (
      (
        uniqueId,
        Newpair(transformExpr(fst), transformExpr(snd), Dummy_Pos)
      )
    )
    uniqueId
  }

  /* Adds the arrayLiter ARR to the key value hash map. Additionally adds each
     array element to the dict and key value hash map. */
  private def addArrayLiterToHashMap(
      arr: ArrayLiter,
      originalStr: VarName
  ): VarName = {
    val uniqueId = addToDict(originalStr)
    kvs += ((uniqueId, arr))
    arr match {
      case ArrayLiter(Some(es), _) =>
        for (i <- es.indices) {
          val aeName = addToDict(originalStr + "-" + i)
          kvs += ((aeName, transformExpr(es(i))))
        }
      case _ =>
    }
    uniqueId
  }

  /* Adds the pair-elem to the kvs hash map or NullRef if a null pointer runtime
     error. */
  private def addPairElemOrNull(
      uniqueId: VarName,
      rhsId: Ident,
      isFst: Boolean
  ): Unit = {
    val Ident(varName, nullRefPos) = rhsId
    val Ident(rhsUniqueId, _) = updateIdent(rhsId)
    kvs.get(rhsUniqueId) match {
      // Derefencing a null pointer runtime error
      case Some(_: PairLiter) => kvs += ((uniqueId, NullRef(nullRefPos)))
      case _ =>
        kvs += (
          (
            uniqueId,
            getLatestHeapExpr(varName, isFst)
          )
        )
    }
  }

  /* Updates the value for a PairElem PAIRELEM in the key value hashmap. */
  private def addPairElemToHashMap(
      pairelem: PairElem,
      originalStr: VarName
  ): VarName = {
    val uniqueId = addToDict(originalStr)
    pairelem match {
      case Fst(rhsId @ Ident(varName, _), _) =>
        addPairElemOrNull(uniqueId, rhsId, Is_Fst)
      case Snd(rhsId @ Ident(varName, _), _) =>
        addPairElemOrNull(uniqueId, rhsId, Not_Fst)
      case _ => ???
    }
    uniqueId
  }

  /* Updates the dict for a new pointer and transforms the arguments for a
     given memory allocation. */
  private def addPointerToHashMap(
      memAlloc: MemoryAlloc,
      originalStr: VarName
  ): (VarName, AssignRHS) = {
    val uniqueId = addToDict(originalStr)
    val updatedRhs = memAlloc match {
      case Realloc(ptr, size, pos) =>
        Realloc(updateIdent(ptr), transformExpr(size), pos)
      case alloc => alloc.map(transformExpr)
    }
    (uniqueId, updatedRhs)
  }

  /* If the hashmap doesn't contain the identifier STR, then it is added to
     the hashmap with initial values (1, 1). If the hashmap contains the
     identifier STR, then the values are updated for unique identifying. */
  private def addToHashMap(id: Ident, rhs: AssignRHS): (Ident, AssignRHS) = {
    val Ident(originalStr, pos) = id
    var uniqueId: VarName = ""
    var updatedRhs = rhs
    rhs match {
      case e: Expr =>
        updatedRhs = transformExpr(e)
        uniqueId = addToDict(originalStr)
        kvs += ((uniqueId, updatedRhs))
      case arr: ArrayLiter =>
        uniqueId = addArrayLiterToHashMap(arr, originalStr)
      case newpair: Newpair =>
        uniqueId = addNewPairToHashMap(newpair, originalStr)
      case pairelem: PairElem =>
        uniqueId = addPairElemToHashMap(pairelem, originalStr)
        updatedRhs = kvs(uniqueId)
      case call: Call =>
        updatedRhs = call.map(transformExpr)

        uniqueId = addToDict(originalStr)
      case memAlloc: MemoryAlloc =>
        val (updatedPtrId, updatedPtrRhs) =
          addPointerToHashMap(memAlloc, originalStr)
        uniqueId = updatedPtrId
        updatedRhs = updatedPtrRhs
    }
    (Ident(uniqueId, pos), updatedRhs)
  }

  /* Add the string s to dict hashmap. If already present then variable counter
     is incremented by 1 and updated in dict. The string for the next unique
     identifier is returned. */
  private def addToDict(s: VarName): VarName = {
    var (x, y, z) =
      dict.getOrElse(s, (Initial_Id_Num, Initial_Id_Num, Initial_Id_Num))
    x += 1
    y = x
    z += 1
    dict += ((s, (x, y, z)))
    x.toString + s
  }

  /* Updates an identifier, prepending the RHS unique ID.
     PRE: the identifier is already in the hashmap DICT. */
  private def updateIdent(id: Ident): Ident = {
    val Ident(originalStr, pos) = id
    val baseStr = strip(originalStr)
    val (uniqueNum, curAssignmentNum, _) = dict(baseStr)
    Ident(curAssignmentNum.toString + baseStr, pos)
  }

  /* Returns true if ID is a heap variable, used to update heap variables with
     shared reference correctly. */
  private def isHeapVariable(id: Ident): Boolean =
    id.getType(currSTable) match {
      case _: ArrayT | _: Pair | _: PtrT => true
      case _                             => false
    }

  /* Transforms an ident ID into SSA form and applies constant propagation. */
  private def transformExprId(id: Ident): Expr = {
    val Ident(s, _) = id
    val uniqueId @ Ident(x, _) = updateIdent(id)
    val rhs = kvs.getOrElse(x, uniqueId)
    rhs match {
      // If kvs returns a different updated ident name (while case) then
      // update, otherwise return prev updated ident
      // If isHeapVariable is true then id is a shared reference so return
      // ident from kvs (original heap variable)
      case Ident(varName, _) =>
        if (varName.endsWith(s) || isHeapVariable(id)) toExpr(rhs) else uniqueId
      case _: Call       => uniqueId
      case _: ArrayLiter => uniqueId
      case _: Newpair    => uniqueId
      case _             => toExpr(rhs)
    }
  }

  /* Gets size of the array the VARNAME is associated with */
  private def getSize(varName: String): Int = {
    val (uniqueNum, curAssignmentNum, _) = dict(varName)
    var lookupStr = varName
    lookupStr = curAssignmentNum.toString + varName

    kvs.get(lookupStr) match {
      case Some(arrayLiter: ArrayLiter) => arrayLiter.len
      case Some(p) =>
        updateRHS(p, varName) match {
          case id @ Ident(str, _) =>
            if (varName == strip(str)) {
              return 0
            }
            getSize(strip(str))
          case _ => ???
        }
      case _ =>
        var length = 0
        while (kvs.contains(lookupStr + "-" + length)) {
          length += 1
        }
        length
    }
  }

  /* Transforms an array-elem AE to SSA form. */
  private def transformExprArrayElem(ae: ArrayElem): Expr = {
    val ArrayElem(id @ Ident(s, _), es, pos) = ae
    var tempId = id
    var Ident(tempStr, _) = id
    var retVal: Expr = id
    val transIndices = ListBuffer.empty[Expr]
    if (id.getType(currSTable).isPtr) {
      return ArrayElem(updateIdent(id), es.map(transformExpr), Dummy_Pos)
    }
    for (i <- es.indices) {
      val index = es(i)
      val transformedExpr = transformExpr(index)
      transIndices += transformedExpr
      // Check for Out of Bounds errors
      val arrSize = getSize(strip(tempStr))
      transformedExpr match {
        case IntLiter(n, pos) if n < 0 || n >= arrSize => return Bounds(pos)
        case _                                         =>
      }
      if (containsIdent(transformedExpr)) {
        tempStr = (arrayElemIdentifier(tempStr, List(index)))
        val Ident(updatedStr, _) = updateIdent(Ident(tempStr, pos))
        kvs.get(updatedStr) match {
          case Some(Ident(sNew, _)) => tempStr = sNew
          case None                 => retVal
          case Some(p)              => retVal = toExpr(p)
        }
      } else {
        val (transformed, toTransform) = es.splitAt(i + 1)
        transIndices ++= toTransform.map(transformExpr)
        return ArrayElem(updateIdent(id), transIndices.toList, pos)
      }
    }
    retVal
  }

  /* Checks if there is an Ident in the expression E */
  private def containsIdent(e: Expr): Boolean = {
    val ids = ListBuffer.empty[Ident]
    getIdent(e, ids)
    ids.isEmpty
  }

  /* Get's Idents used in E. */
  private def getIdent(e: Expr, ids: ListBuffer[Ident]): ListBuffer[Ident] = {
    e match {
      case id: Ident => ids += id
      case binOp: BinOp =>
        getIdent(binOp.lExpr, ids)
        getIdent(binOp.rExpr, ids)
      case unOp: UnOp =>
        getIdent(unOp.e, ids)
      case _ => ids
    }
  }

  /* Transforms an expression E into SSA form and applies constant folding &
     propagation. */
  private def transformExpr(e: Expr): Expr = e match {
    case sizeof: SizeOf     => sizeof
    case id: Ident          => transformExprId(id)
    case Addr(id: Ident, _) => Addr(updateIdent(id), Dummy_Pos)
    case ae @ ArrayElem(id, es, pos) =>
      if (id.getType(currSTable).isPtr) {
        ArrayElem(updateIdent(id), es.map(transformExpr), pos)
      } else {
        transformExprArrayElem(ae)
      }
    case liter: Liter => liter
    case Len(exp, pos) =>
      exp match {
        case id @ Ident(str, _) =>
          val Ident(varName, _) = transformExpr(id)
          kvs(varName) match {
            case arrayLiter: ArrayLiter => IntLiter(arrayLiter.len, pos)
            case _                      => ???
          }
        case ArrayElem(id, exprs, _) => fold(e.map(transformExpr))
        // Semantically incorrect
        case _ => ???
      }
    // All other operators
    case _ => fold(e.map(transformExpr))
  }

  /* Transforms an expression E for the condition of a while loop. */
  private def transformExpr(e: Expr, map: Map[VarName, NumOfAssigns]): Expr =
    e match {
      case sizeof: SizeOf   => sizeof
      case id @ Ident(s, p) =>
        // If ident not present in map, it is not changed in while loop so,
        // transformExpr as usual
        map.get(s) match {
          case Some(assignmentNum) =>
            // Update id to use phi var
            val (_, curId, _) = dict(s)
            Ident((assignmentNum + curId).toString + s, p)
          case None => transformExpr(id)
        }
      case ae: ArrayElem => transformExpr(ae)
      case liter: Liter  => liter
      // All operators
      case _ => fold(e.map(exp => transformExpr(exp, map)))
    }

  /* Potentially removes identifiers that are no longer required due future
     constant propagation. */
  private def deadCodeElimination(
      rhs: AssignRHS,
      t: Type,
      ident: Ident,
      buf: ListBuffer[Stat]
  ): ListBuffer[Stat] = {
    rhs match {
      case err: Runtime =>
        buf += RuntimeErr(err)
      case memAlloc: MemoryAlloc if t.isPtr =>
        stackSize += getBaseTypeSize(t)
        buf += EqIdent(t, ident, rhs)
      case _: Expr | _: ArrayLiter | _: Newpair | _: PairElem => buf
      case _ =>
        stackSize += getBaseTypeSize(t)
        buf += EqIdent(t, ident, rhs)
    }
  }

  /* Changes a given assign-rhs RHS into an Expr. */
  private def toExpr(rhs: AssignRHS): Expr = rhs match {
    case e: Expr => e
    // invalid
    case _ => ???
  }

  /* Creates a unique identifier for the element at position ES of array VARNAME */
  private def arrayElemIdentifier(varName: VarName, es: List[Expr]): VarName = {
    varName + "-" + es.map(transformExpr).mkString("-")
  }

  /* Creates a unique identifier for an element of pair VARNAME*/
  private def pairElemIdentifier(varName: VarName, isFst: Boolean): VarName = {
    val pairElemType = if (isFst) "fst" else "snd"
    varName + "-" + pairElemType
  }

  /* Transforms a read stat to SSA. */
  private def transformRead(lhs: AssignLHS): ListBuffer[Stat] = {
    var e: Expr = null
    val varName = lhs match {
      case id @ Ident(s, _) =>
        e = toExpr(kvs(updateIdent(id).s))
        s
      case ae @ ArrayElem(Ident(s, _), es, _) =>
        transformExprArrayElem(ae) match {
          case bounds: Bounds => return ListBuffer(RuntimeErr(bounds))
          case _              =>
        }
        val aeName = arrayElemIdentifier(s, es)
        e = getLatestHeapExpr(aeName)
        aeName
      case Fst(Ident(s, _), _) =>
        val fstName = pairElemIdentifier(s, Is_Fst)
        e = getLatestHeapExpr(fstName)
        fstName
      case Snd(Ident(s, _), _) =>
        val sndName = pairElemIdentifier(s, Not_Fst)
        e = getLatestHeapExpr(sndName)
        sndName
      case deref: DerefPtr =>
        e = transformExpr(deref)
        ""
      // Semantically incorrect
      case _ => ???
    }
    e match {
      case nullRef: NullRef => ListBuffer(RuntimeErr(nullRef))
      case deref: DerefPtr  => ListBuffer(Read(deref))
      // If lhs evaluates to an ident then use that var for read
      case id: Ident =>
        val (_, curAssignmentNum, _) = dict(varName)
        // If read occurs within while loop and phi var is involved, increment
        // dict
        if (curAssignmentNum.toString + varName != id.s) {
          addToDict(varName)
        }
        ListBuffer(Read(id))
      // Otherwise introduce next variable in dict for read
      case _ =>
        // add to dict but not kvs
        val uniqueId = Ident(addToDict(varName), Undefined_Value)
        val t = getExprType(e)
        stackSize += getBaseTypeSize(t)
        ListBuffer(EqIdent(t, uniqueId, e), Read(uniqueId))
    }
  }

  /* Counts the number of assignments a variable previously declared in a higher
     scope has within an IF, ELSE or WHILE statement. */
  private def countVariables(
      stat: Stat,
      map: Map[VarName, NumOfAssigns],
      mapScope: Map[VarName, NumOfAssigns]
  ): Unit = {
    stat match {
      case EqAssign(Ident(s, _), _) => incrementMap(s, map, mapScope)
      case EqAssign(ArrayElem(Ident(s, _), elems, _), _) =>
        incrementMap(s, map, mapScope)
      case EqAssign(Fst(Ident(s, _), _), _) =>
        incrementMap(pairElemIdentifier(s, Is_Fst), map, mapScope)
      case EqAssign(Snd(Ident(s, _), _), _) =>
        incrementMap(pairElemIdentifier(s, Not_Fst), map, mapScope)
      case Read(Ident(s, _)) =>
        incrementMap(s, map, mapScope)
      case Read(ArrayElem(Ident(s, _), elems, _)) =>
        incrementMap(s, map, mapScope)
      case Read(Fst(Ident(s, _), _)) =>
        incrementMap(pairElemIdentifier(s, Is_Fst), map, mapScope)
      case Read(Snd(Ident(s, _), _)) =>
        incrementMap(pairElemIdentifier(s, Not_Fst), map, mapScope)
      case Seq(stats) =>
        for (s <- stats) {
          countVariables(s, map, mapScope)
        }
      case _ =>
    }
  }

  /* Function called by countVariables to increment the number for each
     variable if and only if it has been declared in higher scope. */
  private def incrementMap(
      s: VarName,
      map: Map[VarName, NumOfAssigns],
      mapScope: Map[VarName, NumOfAssigns]
  ): Unit = {
    // Only add to map if already declared in higher scope
    if (dict.contains(s)) {
      // map is for both if and else branches
      map(s) = map.getOrElseUpdate(s, Initial_Id_Num) + 1
      // mapScope is map for individual if or else branch
      if (mapScope != Undefined_Map) {
        mapScope(s) = mapScope.getOrElseUpdate(s, Initial_Id_Num) + 1
      }
    }
  }

  private def getIdentsAssigRHS(
      rhs: AssignRHS,
      ids: ListBuffer[Ident]
  ): ListBuffer[Ident] = {
    rhs match {
      case id @ Ident(str, _) =>
        val updatedId @ Ident(varName, _) = updateIdent(id)
        kvs.get(varName) match {
          case Some(assignRHS) =>
            ids += updatedId
            getIdentsAssigRHS(assignRHS, ids)
          case _ =>
            ids += updatedId
        }
      case e: Expr => getIdent(e, ids)
      case al @ ArrayLiter(Some(elems), _) =>
        ids ++= elems.flatMap(e => getIdent(e, ids))
      case Newpair(fst, snd, _) =>
        getIdentsAssigRHS(fst, ids)
        getIdentsAssigRHS(snd, ids)
      case Fst(e, _) => ids ++= getIdentsAssigRHS(e, ids)
      case Snd(e, _) => ids ++= getIdentsAssigRHS(e, ids)
      case Call(id, Some(ArgList(params)), pos) =>
        ids ++= params.flatMap(e => getIdent(e, ids))
      case _ => ids
    }
  }

  private def addUndeclaredVars(rhs: AssignRHS, ids: ListBuffer[Ident]) {
    val buf = ListBuffer.empty[Stat]
    getIdentsAssigRHS(rhs, ids)
    for (id <- ids.reverse) {
      if (!declaredHeapVars.contains(id)) {
        val Ident(str, _) = id
        declaredHeapVars += id
        if (kvs.contains(str)) {
          buf += EqIdent(
            Ident(strip(str), Dummy_Pos).getType(sTable),
            id,
            kvs(str)
          )
        }
      }
    }
  }

  /* If a variable from higher scope is changed within an if or else branch,
     introduce new variable before if statement to account for phi. */
  private def createPhiVar(x: (VarName, NumOfAssigns)): ListBuffer[Stat] = {
    // n is the number of assignments in both if and else branches
    val (s, numOfAssigns) = x
    // y is the current number of assignments for the given variable
    val (_, curAssignmentNum, _) = dict(s)
    val varName = curAssignmentNum.toString + s
    // Get previously defined value from kvs
    val rhs = kvs.getOrElse(varName, Ident(varName, Undefined_Value))
    var originalType = currSTable.lookupAllType(Ident(s, Undefined_Pos))
    stackSize += getBaseTypeSize(originalType)
    val buf = ListBuffer.empty[Stat]
    val ids = ListBuffer.empty[Ident]
    addUndeclaredVars(rhs, ids)
    buf +=
      EqIdent(
        originalType,
        Ident((numOfAssigns + curAssignmentNum).toString + s, Undefined_Pos),
        rhs
      )
  }

  /* Updates the PHI variable, given the map of the condition branch */
  private def updatePhiVar(
      x: (VarName, NumOfAssigns),
      map: Map[VarName, NumOfAssigns]
  ): Stat = {
    val (varName, _) = x
    val (_, curAssignmentNum, _) = dict(varName)
    val uniqueId = curAssignmentNum.toString + varName
    val rhs = kvs.getOrElse(uniqueId, Ident(uniqueId, Undefined_Pos))
    var lhs = Ident(uniqueId, Undefined_Pos)
    if (map != Undefined_Map) {
      lhs = Ident(
        (map.getOrElse(
          varName,
          Initial_Id_Num
        ) + curAssignmentNum).toString + varName,
        Undefined_Pos
      )
    }
    // LHS = RHS if the phi var is used for a read, therefore EqAssign is
    // redundant
    if (lhs != rhs) {
      EqAssign(lhs, rhs)
    } else {
      Skip
    }
  }

  /* Transforms an if stat into SSA form, using PHI functions. */
  private def transformIf(cond: Expr, s1: Stat, s2: Stat): ListBuffer[Stat] = {
    val stats = ListBuffer.empty[Stat]
    // Number of re-assignments of pre-declared vars in IF and ELSE branches
    val map = Map.empty[VarName, NumOfAssigns]
    // Number of re-assignments of pre-declared vars in IF branch
    val mapIf = Map.empty[VarName, NumOfAssigns]
    // Number of re-assignments of pre-declared vars in ELSE branch
    val mapElse = Map.empty[VarName, NumOfAssigns]
    // Count var re-assignments into map, mapIf & mapElse
    countVariables(s1, map, mapIf)
    countVariables(s2, map, mapElse)
    // Create the PHI variables if necessary
    val mapList = map.toList
    stats ++= mapList.flatMap(createPhiVar)
    // Transform S1 and S2 and updatePhiVar within each branch (if necessary)
    val e = transformExpr(cond)
    e match {
      // control flow analysis
      case BoolLiter(true, _)  => transformStat(s1)
      case BoolLiter(false, _) => transformStat(s2)
      // unable to determine condition value
      case _ =>
        currSTable = currSTable.getNextScopeSSA
        val ssa1 =
          transformStat(s1) ++ mapIf.toList
            .map(x => updatePhiVar(x, mapElse))
            .filter(x => x != Skip)
        currSTable = currSTable.getPrevScope
        currSTable = currSTable.getNextScopeSSA
        val ssa2 =
          transformStat(s2) ++ mapElse.toList
            .map(x => updatePhiVar(x, Undefined_Map))
            .filter(x => x != Skip)
        currSTable = currSTable.getPrevScope
        val ifStat = If(e, Seq(ssa1.toList), Seq(ssa2.toList))
        // Remove phi var from kvs so if called upon later, ident is used
        mapList.foreach(x => {
          val (_, curAssignmentNum, _) = dict(x._1)
          kvs -= curAssignmentNum.toString + x._1
        })
        // dead code elimination
        if (ssa1.isEmpty && ssa2.isEmpty) stats else stats += ifStat
    }
  }

  /* TODO */
  private def getPhiVar(x: (VarName, NumOfAssigns)): (VarName, Ident) = {
    val (varName, numOfAssigns) = x
    val (_, curAssignmentNum, _) = dict(varName)
    (
      varName,
      Ident((numOfAssigns + curAssignmentNum).toString + varName, Undefined_Pos)
    )
  }

  /* TODO */
  private def transformWhile(cond: Expr, s: Stat): ListBuffer[Stat] = {
    val stats = ListBuffer.empty[Stat]
    // Number of re-assignments of pre-declared vars within WHILE
    val map = Map.empty[VarName, NumOfAssigns]
    // Count var re-assignments into map
    countVariables(s, map, Undefined_Map)
    // Create the PHI variables if necessary
    val mapList = map.toList
    stats ++= mapList.flatMap(createPhiVar)
    // Transform the while condition, updating with the phi vars if necessary
    // Transform the inner of the while, updating phi vars if necessary
    val e = transformExpr(cond, map)
    e match {
      case BoolLiter(false, _) => stats
      case _                   =>
        // Updates variables in kvs to phi variables
        map
          .map(getPhiVar)
          .foreach(tup => {
            val (varName, id) = tup
            val (_, curAssignmentNum, _) = dict(varName)
            kvs += ((curAssignmentNum.toString + varName, id))
          })

        currSTable = currSTable.getNextScopeSSA
        val ssa =
          transformStat(s) ++ mapList
            .map(x => updatePhiVar(x, Undefined_Map))
            .filter(x => x != Skip)
        currSTable = currSTable.getPrevScope
        // Remove phi var from kvs so if called upon later, ident is used
        mapList.foreach(x => {
          val (varName, _) = x
          val (_, curAssignmentNum, _) = dict(varName)
          kvs -= curAssignmentNum.toString + varName
        })
        stats += While(e, Seq(ssa.toList))
        stats
    }
  }

  /* Strips initial Numbers from a string*/
  private def strip(str: String): String = {
    str.dropWhile(c => c.isDigit)
  }

  /* Function used to retrieve the latest value of a heap variable in kvs.
     If not present in kvs then it returns the most recent ident associated
     with the given heap variable. */
  private def getLatestHeapExpr(elemName: VarName): Expr = {
    if (!dict.contains(elemName)) {
      NullRef(Undefined_Pos)
    } else {
      val (uniqueNum, curAssignmentNum, _) = dict(strip(elemName))
      val lookupName =
        // if (uniqueNum == 0) {
        curAssignmentNum.toString + strip(elemName)
      // } else {
      //   uniqueNum.toString + strip(elemName)
      // }
      toExpr(kvs.getOrElse(lookupName, Ident(lookupName, Undefined_Pos)))
    }
  }

  private def getLatestHeapExpr(elemName: VarName, is_fst: Boolean): Expr = {
    val varName = pairElemIdentifier(elemName, is_fst)
    if (dict.contains(pairElemIdentifier(elemName, is_fst))) {
      getLatestHeapExpr(varName)
    } else {
      val (uniqueNum, curAssignmentNum, _) = dict(strip(elemName))
      val lookupName =
        // if (uniqueNum == 0) {
        curAssignmentNum.toString + strip(elemName)
      // } else {
      // uniqueNum.toString + elemName
      // }
      kvs.get(lookupName) match {
        case Some(Ident(str, _)) =>
          getLatestHeapExpr(pairElemIdentifier(strip(str), is_fst))
        case _ =>
          NullRef(Undefined_Pos)
      }
    }
  }

  /* Updates the elements of an arrayLiter using the updated arrayElem values
     from the kvs. */
  private def updateArrayLiter(
      elems: List[Expr],
      pos: (Line, Col),
      arrName: VarName
  ): AssignRHS = {
    val updatedElems = ListBuffer.empty[Expr]
    // Gets most up to date array elem values from kvs or its corresponding
    // variable name if not present in kvs
    for (i <- elems.indices) {
      updatedElems += getLatestHeapExpr(arrName + "-" + i)
    }
    val updatedArr = ArrayLiter(Some(updatedElems.toList), pos)
    // Only update the arrayLiter in kvs for variable arrStr if necessary
    if (updatedElems.toList != elems) {
      addToDict(arrName)
    } else {
      val (_, curAssignNum, _) = dict(arrName)
      kvs -= curAssignNum.toString + arrName
    }
    updatedArr
  }

  /* Updates the elements of a Newpair using the updated fst and snd values
     from the kvs. */
  private def updateNewpair(pairName: VarName, oldPair: Newpair): AssignRHS = {
    val Newpair(_, _, pos) = oldPair
    // Gets most up to date values for fst and snd from kvs
    val updatedFst = getLatestHeapExpr(pairName + "-fst")
    val updatedSnd = getLatestHeapExpr(pairName + "-snd")
    val updatedPair = Newpair(updatedFst, updatedSnd, pos)
    // Only update newpair in kvs if necessary
    if (updatedPair != oldPair) {
      kvs += ((addToDict(pairName), updatedPair))
    }
    updatedPair
  }

  /* TODO */
  private def updateRHS(rhs: AssignRHS, varName: VarName): AssignRHS =
    rhs match {
      case ArrayLiter(Some(es), pos) =>
        updateArrayLiter(es, pos, varName)
      // Empty arrayLiter case, nothing to update
      case ArrayLiter(None, pos) => ArrayLiter(None, pos)
      case pair: Newpair         => updateNewpair(varName, pair)
      case call: Call            => call
      case id: Ident             => transformExpr(id)
      case e: Expr               => e
      case _                     => ???
    }

  /* TODO */
  private def transExpArray(e: Expr, pf: (Expr => Stat)): ListBuffer[Stat] = {
    val buf = ListBuffer.empty[Stat]
    e match {
      case id @ Ident(varName, _) =>
        var uniqueId @ Ident(x, _) = updateIdent(id)
        val rhs = kvs.getOrElse(x, uniqueId)
        rhs match {
          // Address operator requires a variable stored in memory
          case addr @ Addr(addrId @ Ident(addrStr, _), _) =>
            val rhsToAdd = kvs(addrStr)
            val t = rhsToAdd.getType(currSTable)
            t match {
              case _: PtrT =>
              case _ =>
                buf += EqIdent(rhs.getType(currSTable), addrId, rhsToAdd)
            }
            buf += pf(Addr(addrId, Dummy_Pos))
          // Not array, transformExpr as usual
          case e: Expr => buf += pf(e)
          // Heap variable has not been defined in current ast then add to list
          // of statements
          case r =>
            val rhsUpdated = updateRHS(r, varName)
            // Get latest ident as it may be updated
            val t = currSTable.lookupAllType(id)
            stackSize += getBaseTypeSize(t)
            transformExpr(id) match {
              case ident: Ident =>
                uniqueId = ident
                buf += EqIdent(t, uniqueId, rhsUpdated)
                buf += pf(uniqueId)
              case _ =>
            }
        }

      // Not ident so not heap variable, transformExpr as usual
      case _ => buf += pf(transformExpr(e))
    }
    buf
  }

  /* Partial function to retrieve runtime expression from inside a statement. */
  private def getRuntimeErr: PartialFunction[Stat, Stat] = {
    case EqIdent(_, _, err: Runtime) => RuntimeErr(err)
    case EqAssign(_, err: Runtime)   => RuntimeErr(err)
    case Free(err: Runtime)          => RuntimeErr(err)
    case Return(err: Runtime)        => RuntimeErr(err)
    case Exit(err: Runtime)          => RuntimeErr(err)
    case Print(err: Runtime)         => RuntimeErr(err)
    case PrintLn(err: Runtime)       => RuntimeErr(err)
    case If(err: Runtime, _, _)      => RuntimeErr(err)
    case While(err: Runtime, _)      => RuntimeErr(err)
    case runtimeErr: RuntimeErr      => runtimeErr
  }

  /* Transforms and equal assignment pair-elem to SSA form & removes dead
     code. */
  private def transformEqAssignPairElem(
      pe: PairElem,
      varName: VarName,
      pos: (Line, Col),
      rhs: AssignRHS,
      isFst: Boolean
  ): ListBuffer[Stat] = {
    val stats = ListBuffer.empty[Stat]
    val Ident(pairId, _) = updateIdent(Ident(varName, Dummy_Pos))
    kvs(pairId) match {
      case PairLiter(pos) => stats += RuntimeErr(NullRef(pos))
      case _ =>
        val (uniqueId, updatedRhs) =
          addToHashMap(Ident(pairElemIdentifier(varName, isFst), pos), rhs)
        deadCodeElimination(updatedRhs, pe.getType(currSTable), uniqueId, stats)
    }
  }

  /* Transforms an equal assignemnt stat to SSA form & removes dead code. */
  private def transformEqAssignStat(assign: EqAssign): ListBuffer[Stat] = {
    val buf = ListBuffer.empty[Stat]
    assign match {
      case EqAssign(id: Ident, r) =>
        val (v, rhs) = addToHashMap(id, r)
        deadCodeElimination(rhs, id.getType(currSTable), v, buf)
      case EqAssign(ae @ ArrayElem(id @ Ident(varName, pos), es, _), r) =>
        // Array out of bounds check, if out of bounds return runtime error
        transformExprArrayElem(ae) match {
          case bounds: Bounds =>
            buf += RuntimeErr(bounds)
          case expr =>
            val newIndices = es.map(transformExpr)
            if (newIndices.map(containsIdent).reduce((b1, b2) => b1 || b2)) {
              addToHashMap(
                Ident(arrayElemIdentifier(strip(varName), newIndices), pos),
                r
              )
            } else {
              addToDict(strip(varName))
            }
            val newId @ Ident(str, _) = updateIdent(id)
            val newArrayElem = ArrayElem(newId, newIndices, pos)
            val rhsNew = updateRHS(r, str)
            if (!kvs.contains(str)) {
              buf += EqAssign(newArrayElem, rhsNew)
            }
            buf
        }
      // PairElem cases
      case EqAssign(fst @ Fst(Ident(varName, pos), _), r) =>
        transformEqAssignPairElem(fst, varName, pos, r, Is_Fst)
      case EqAssign(snd @ Snd(Ident(varName, pos), _), r) =>
        transformEqAssignPairElem(snd, varName, pos, r, Not_Fst)
      case EqAssign(derefPtr: DerefPtr, r) =>
        val updatedRhs = r match {
          case e: Expr => transformExpr(e)
          case _       => r
        }
        buf += EqAssign(derefPtr.map(transformExpr), updatedRhs)
      // Semantically incorrect
      case _ => ???
    }
  }

  /* Transforms a sequential statement to SSA form and removes any dead code
     after Exit or Return statements. */
  private def transformSeqStat(stats: List[Stat]): ListBuffer[Stat] = {
    val buf = ListBuffer.empty[Stat]
    var stop = false
    var i = 0
    while (!stop && i < stats.length) {
      var curStat = transformStat(stats(i))
      if (!curStat.isEmpty) {
        curStat.last match {
          case stat if getRuntimeErr.isDefinedAt(stat) =>
            stop = true
            curStat = ListBuffer(getRuntimeErr(stat))
          // dead code elimination
          case _: Return | _: Exit => stop = true
          case _                   => // otherwise continue
        }
      }
      i += 1
      buf ++= curStat
    }
    buf
  }

  /* Transforms a free statement into SSA form and checks for null runtime
     errors. */
  private def transformFree(stat: Stat): ListBuffer[Stat] = {
    val Free(e) = stat
    // Null reference runtime error
    transExpArray(e, Free).map(s =>
      s match {
        case Free(PairLiter(pos)) => RuntimeErr(NullRef(pos))
        case _                    => s
      }
    )
  }

  /* Transforms a given statement S into SSA form. */
  private def transformStat(stat: Stat): ListBuffer[Stat] = {
    val buf = ListBuffer.empty[Stat]
    stat match {
      case EqIdent(t, id, r) =>
        val (v, rhs) = addToHashMap(id, r)
        scopeRedefine(id)
        deadCodeElimination(rhs, t, v, buf)
      case eqAssign: EqAssign => transformEqAssignStat(eqAssign)
      case Read(lhs)          => transformRead(lhs)
      case _: Free            => transformFree(stat)
      case Return(e)          => transExpArray(e, Return)
      case Exit(e)            => buf += Exit(transformExpr(e))
      case Print(e)           => transExpArray(e, Print)
      case PrintLn(e)         => transExpArray(e, PrintLn)
      case If(cond, s1, s2)   => transformIf(cond, s1, s2)
      case Seq(statList)      => transformSeqStat(statList)
      case Skip               => buf
      case While(cond, stat)  => transformWhile(cond, stat)
      case Begin(stat) =>
        val oldScopes = dict.map(enteringScope)
        dict = dict.map(foo)
        currSTable = currSTable.getNextScopeSSA
        val transformedS = transformStat(stat)
        currSTable = currSTable.getPrevScope
        dict = dict.map(x => leavingScope(x, oldScopes))
        transformedS
      case stat => buf += stat
    }
  }

  /* Update the scope assignment counter to 1 if variable is redeclared
     (eqIdent) in scope  */
  def scopeRedefine(id: Ident): Unit = {
    val Ident(varName, _) = id
    var (uniqueId, curAssignmentNum, _) = dict(varName)
    dict += ((varName, (uniqueId, curAssignmentNum, Initial_Id_Num + 1)))
  }

  /* TODO */
  def enteringScope(kv: (VarName, IdNums)): (VarName, Int) = {
    val (varName, (uniqueId, curAssignmentNum, numOfAssigns)) = kv
    (varName, numOfAssigns)
  }

  def foo(kv: (VarName, IdNums)): (VarName, IdNums) = {
    val (varName, (uniqueId, curAssignmentNum, numOfAssigns)) = kv
    (varName, (uniqueId, curAssignmentNum, Initial_Id_Num))
  }

  /* Update the current var number to correct value when leaving scope.
     Reset the scope counter to 0 when leaving scope. */
  def leavingScope(
      kv: (VarName, IdNums),
      oldScopes: HashMap[VarName, Int]
  ): (VarName, IdNums) = {
    val (varName, (uniqueId, curAssignmentNum, numOfAssigns)) = kv
    var oldAssignNum = oldScopes.get(varName) match {
      case Some(n) => n
      case None    => Initial_Id_Num
    }
    (varName, (uniqueId, curAssignmentNum - numOfAssigns, oldAssignNum))
  }

  /* Transforms a given function F into SSA form. */
  def transformFunc(f: Func): (Func, VarStackSize) = {
    val Func(t, id, params, stat) = f
    val newParams =
      params.map(pList =>
        pList.map(i => {
          val Ident(v, pos) = i
          Ident(addToDict(v), pos)
        })
      )
    currSTable = currSTable.getNextScopeSSA
    val ssaStat = transformStat(stat)
    currSTable = currSTable.getPrevScope
    val funcStackSize = stackSize
    stackSize = 0
    (Func(t, id, newParams, Seq(ssaStat.toList)), funcStackSize)
  }

  /* Function used to filter for all the RuntimeErrs detected at compile
     time. */
  private def isRuntimeErr(s: Stat): Boolean = s match {
    case _: RuntimeErr => true
    case _             => false
  }

  /* Transforms a given program AST into SSA form. */
  def toSSA(
      ast: Program
  ): (Program, ListBuffer[Stat], List[VarStackSize]) = {
    val Program(fs, stat) = ast
    val (funcs, funcStackSizes) = fs.map(transformFunc).unzip
    val transformedStats = transformStat(stat)
    val p = Program(funcs, Seq(transformedStats.toList))
    val stackSizes = funcStackSizes :+ stackSize
    println(p)
    // printHash()
    (p, transformedStats.filter(isRuntimeErr), stackSizes)
  }

  def printHash(): Unit = {
    println("\n\nKVS IS COMING")
    for (e <- kvs) {
      println(e)
    }
    println("END\n\n")
  }

  def printDict(): Unit = {
    println("\n\nDICT IS COMING")
    for (e <- dict) {
      println(e)
    }
    println("END\n\n")
  }
}
