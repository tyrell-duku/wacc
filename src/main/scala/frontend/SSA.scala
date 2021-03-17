package frontend

import Rules._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import backend.CodeGenerator.{getExprType, getBaseTypeSize}
import frontend.Semantics.SymbolTable
import frontend.ConstantFolding.fold

case class SSA(sTable: SymbolTable) {
  /* Type aliases */
  type StackSize = Int
  type UniqueNum = Int
  type CurrentAssignmentNum = Int
  type NumOfAssigns = Int
  // hash map for unique identifying
  private var dict =
    new HashMap[String, (UniqueNum, CurrentAssignmentNum, NumOfAssigns)]
  // (key, values) hash map for constant propagation
  private var kvs = new HashMap[String, AssignRHS]
  private var currSTable = sTable
  /* Constants */
  val Initial_Id_Num = 0
  val Undefined_Value = null
  val Undefined_Pos = null
  val Undefined_Map = null
  val Is_Fst = true
  val Not_Fst = false
  var stackSize = 0

  /* Adds the NewPair NEWPAIR to the key value hash map. */
  private def addNewPairToHashMap(
      newpair: Newpair,
      originalStr: String
  ): String = {
    val uniqueId = addToDict(originalStr)
    kvs += ((uniqueId, newpair))
    val Newpair(fst, snd, _) = newpair
    // Fst elem
    val fstName = addToDict(pairElemIdentifier(originalStr, Is_Fst))
    kvs += ((fstName, transformExpr(fst)))
    // Snd elem
    val sndName = addToDict(pairElemIdentifier(originalStr, Not_Fst))
    kvs += ((sndName, transformExpr(snd)))
    uniqueId
  }

  /* Adds the arrayLiter ARR to the key value hash map. Additionally adds each
     array element to the dict and key value hash map. */
  private def addArrayLiterToHashMap(
      arr: ArrayLiter,
      originalStr: String
  ): String = {
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

  /* Updates the value for a PairElem PAIRELEM in the key value hashmap. */
  private def addPairElemToHashMap(
      pairelem: PairElem,
      originalStr: String
  ): String = {
    val uniqueId = addToDict(originalStr)
    pairelem match {
      case Fst(Ident(s, _), _) =>
        kvs += ((uniqueId, getLatestHeapExpr(pairElemIdentifier(s, Is_Fst))))
      case Snd(Ident(s, _), _) =>
        kvs += ((uniqueId, getLatestHeapExpr(pairElemIdentifier(s, Not_Fst))))
      // Semantically incorrect
      case _ => ???
    }
    uniqueId
  }

  /* If the hashmap doesn't contain the identifier STR, then it is added to
     the hashmap with initial values (1, 1). If the hashmap contains the
     identifier STR, then the values are updated for unique identifying. */
  private def addToHashMap(id: Ident, rhs: AssignRHS): (Ident, AssignRHS) = {
    val Ident(originalStr, pos) = id
    var uniqueId: String = null
    var updatedRhs = rhs
    rhs match {
      case e: Expr =>
        val transEx = transformExpr(e)
        uniqueId = addToDict(originalStr)
        kvs += ((uniqueId, transEx))
      case arr: ArrayLiter =>
        uniqueId = addArrayLiterToHashMap(arr, originalStr)
      case newpair: Newpair =>
        uniqueId = addNewPairToHashMap(newpair, originalStr)
      case pairelem: PairElem => addPairElemToHashMap(pairelem, originalStr)
      case call: Call =>
        updatedRhs = call.map(transformExpr)
        uniqueId = addToDict(originalStr)
      case _ =>
    }
    (Ident(uniqueId, pos), updatedRhs)
  }

  /* Add the string s to dict hashmap. If already present then variable counter
     is incremented by 1 and updated in dict. The string for the next unique
     identifier is returned. */
  private def addToDict(s: String): String = {
    var (x, y, z) =
      dict.getOrElse(s, (Initial_Id_Num, Initial_Id_Num, Initial_Id_Num))
    x += 1
    y += 1
    z += 1
    dict += ((s, (x, y, z)))
    x.toString + s
  }

  /* Updates an identifier, prepending the RHS unique ID.
     PRE: the identifier is already in the hashmap DICT. */
  private def updateIdent(id: Ident): Ident = {
    val Ident(originalStr, pos) = id
    val (_, y, _) = dict(originalStr)
    Ident(y.toString + originalStr, pos)
  }

  /* Transforms an ident ID into SSA form and applies constant propagation. */
  private def transformExprId(id: Ident): Expr = {
    val Ident(s, _) = id
    val id2 @ Ident(x, _) = updateIdent(id)
    val rhs = kvs.getOrElse(x, id2)
    rhs match {
      // If kvs returns a different updated ident name (while case) then
      // update, otherwise return prev updated ident
      case Ident(str, _) =>
        if (str.endsWith(s)) toExpr(rhs) else id2
      case _: Call => id2
      case _       => toExpr(rhs)
    }
  }

  /* Transforms an array-elem AE to SSA form. */
  private def transformExprArrayElem(ae: ArrayElem): Expr = {
    val ArrayElem(id @ Ident(s, _), es, pos) = ae
    val arrayElemName = arrayElemIdentifier(s, es)
    // If elem not in dict then out of bounds
    dict.get(arrayElemName) match {
      case Some((_, y, _)) =>
        toExpr(
          kvs.getOrElse(
            y.toString + arrayElemName,
            Ident(y.toString + arrayElemName, Undefined_Pos)
          )
        )
      case None => Bounds
    }
  }

  /* Transforms an expression E into SSA form and applies constant folding &
     propagation. */
  private def transformExpr(e: Expr): Expr = e match {
    case sizeof: SizeOf => sizeof
    case id: Ident      => transformExprId(id)
    case ae: ArrayElem  => transformExprArrayElem(ae)
    case liter: Liter   => liter
    // All operators
    case _ => fold(e.map(transformExpr))
  }

  /* Transforms an expression E for the condition of a while loop. */
  private def transformExpr(e: Expr, map: Map[String, Int]): Expr = e match {
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

  private def deadCodeElimination(
      rhs: AssignRHS,
      t: Type,
      ident: Ident,
      buf: ListBuffer[Stat]
  ): ListBuffer[Stat] =
    rhs match {
      case _: Expr | _: ArrayLiter | _: Newpair | _: PairElem => buf
      case _ =>
        stackSize += getBaseTypeSize(t)
        buf += EqIdent(t, ident, rhs)
    }

  private def toExpr(rhs: AssignRHS): Expr = rhs match {
    case e: Expr => e
    // invalid
    case _ => ???
  }

  private def arrayElemIdentifier(varName: String, es: List[Expr]): String = {
    varName + "-" + es.map(transformExpr).mkString("-")
  }

  private def pairElemIdentifier(varName: String, isFst: Boolean): String = {
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
      case ArrayElem(id @ Ident(s, _), es, pos) =>
        val aeName = arrayElemIdentifier(s, es)
        e = getLatestHeapExpr(aeName)
        aeName
      case Fst(id @ Ident(s, _), _) =>
        val fstName = pairElemIdentifier(s, Is_Fst)
        e = getLatestHeapExpr(fstName)
        fstName
      case Snd(id @ Ident(s, _), _) =>
        val sndName = pairElemIdentifier(s, Not_Fst)
        e = getLatestHeapExpr(sndName)
        sndName
      // TODO: Deref Ptr
      // Semantically incorrect
      case _ => ???
    }
    e match {
      // If lhs evaluates to an ident then use that var for read
      case id: Ident => ListBuffer(Read(id))
      // Otherwise introduce next variable in dict for read
      case _ =>
        // add to dict but not kvs
        val v = Ident(addToDict(varName), Undefined_Value)
        val t = getExprType(e)
        stackSize += getBaseTypeSize(t)
        ListBuffer(EqIdent(t, v, e), Read(v))
    }
  }

  /* Counts the number of assignments a variable previously declared in a higher
     scope has within an IF, ELSE or WHILE statement. */
  private def countVariables(
      stat: Stat,
      map: Map[String, Int],
      mapScope: Map[String, Int]
  ): Unit = {
    stat match {
      case EqAssign(Ident(s, _), _) => incrementMap(s, map, mapScope)
      case EqAssign(ArrayElem(Ident(s, _), elems, _), _) =>
        incrementMap(arrayElemIdentifier(s, elems), map, mapScope)
      case EqAssign(Fst(Ident(s, _), _), _) =>
        incrementMap(pairElemIdentifier(s, Is_Fst), map, mapScope)
      case EqAssign(Snd(Ident(s, _), _), _) =>
        incrementMap(pairElemIdentifier(s, Not_Fst), map, mapScope)
      case Read(Ident(s, _)) =>
        incrementMap(s, map, mapScope)
      case Read(ArrayElem(Ident(s, _), elems, _)) =>
        incrementMap(arrayElemIdentifier(s, elems), map, mapScope)
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
      s: String,
      map: Map[String, Int],
      mapScope: Map[String, Int]
  ): Unit = {
    // Only add to map if already declared in higher scope
    if (dict.contains(s)) {
      // map is for both if and else branches
      map(s) = map.getOrElseUpdate(s, 0) + 1
      // mapScope is map for individual if or else branch
      if (mapScope != Undefined_Map) {
        mapScope(s) = mapScope.getOrElseUpdate(s, 0) + 1
      }
    }
  }

  private def pruneIf(ssa: ListBuffer[Stat]): Stat = {
    if (ssa.isEmpty) Skip else Seq(ssa.toList)
  }

  /* If a variable from higher scope is changed within an if or else branch,
     introduce new variable before if statement to account for phi. */
  private def createPhiVar(x: (String, Int)): Stat = {
    // n is the number of assignments in both if and else branches
    val (s, n) = x
    // y is the current number of assignments for the given variable
    val (_, y, _) = dict(s)
    val varName = y.toString + s
    // Get previously defined value from kvs
    val rhs = kvs.getOrElse(varName, Ident(varName, Undefined_Value))
    var originalType = currSTable.lookupAllType(Ident(s, Undefined_Pos))
    stackSize += getBaseTypeSize(originalType)
    EqIdent(originalType, Ident((n + y).toString + s, Undefined_Pos), rhs)
  }

  /* Updates the PHI variable, given the map of the condition branch */
  private def updatePhiVar(x: (String, Int), map: Map[String, Int]): Stat = {
    val (s, _) = x
    val (_, y, _) = dict(s)
    val varName = y.toString + s
    val rhs = kvs.getOrElse(varName, Ident(varName, Undefined_Pos))
    if (map != Undefined_Map) {
      EqAssign(
        Ident((map.getOrElse(s, 0) + y).toString + s, Undefined_Pos),
        rhs
      )
    } else {
      EqAssign(Ident(y.toString + s, Undefined_Pos), rhs)
    }
  }

  /* Transforms an if stat into SSA form, using PHI functions. */
  private def transformIf(cond: Expr, s1: Stat, s2: Stat): ListBuffer[Stat] = {
    val stats = ListBuffer.empty[Stat]
    // Number of re-assignments of pre-declared vars in IF and ELSE branches
    val map = Map.empty[String, Int]
    // Number of re-assignments of pre-declared vars in IF branch
    val mapIf = Map.empty[String, Int]
    // Number of re-assignments of pre-declared vars in ELSE branch
    val mapElse = Map.empty[String, Int]
    // Count var re-assignments into map, mapIf & mapElse
    countVariables(s1, map, mapIf)
    countVariables(s2, map, mapElse)

    // Create the PHI variables if necessary
    val mapList = map.toList
    stats ++= mapList.map(createPhiVar)

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
          transformStat(s1) ++ mapIf.toList.map(x => updatePhiVar(x, mapElse))
        currSTable = currSTable.getPrevScope
        currSTable = currSTable.getNextScopeSSA
        val ssa2 =
          transformStat(s2) ++ mapElse.toList.map(x =>
            updatePhiVar(x, Undefined_Map)
          )
        currSTable = currSTable.getPrevScope
        val ifStat = If(e, pruneIf(ssa1), pruneIf(ssa2))

        // Remove phi var from kvs so if called upon later, ident is used
        mapList.foreach((x: (String, Int)) => {
          val (_, y, _) = dict(x._1)
          kvs -= y.toString + x._1
        })

        ifStat match {
          case If(_, Skip, Skip) => stats
          case _                 => stats += ifStat
        }
    }

  }

  private def getPhiVar(x: (String, Int)): (String, Ident) = {
    val (s, n) = x
    // y is the current number of assignments for the given variable
    val (_, y, _) = dict(s)
    (s, Ident((n + y).toString + s, Undefined_Pos))
  }

  private def transformWhile(cond: Expr, s: Stat): ListBuffer[Stat] = {
    val stats = ListBuffer.empty[Stat]
    // Number of re-assignments of pre-declared vars within WHILE
    val map = Map.empty[String, Int]
    // Count var re-assignments into map
    countVariables(s, map, Undefined_Map)

    // Create the PHI variables if necessary
    val mapList = map.toList
    stats ++= mapList.map(createPhiVar)

    // Transform the while condition, updating with the phi vars if necessary
    // Transform the inner of the while, updating phi vars if necessary
    val e = transformExpr(cond, map)

    e match {
      case BoolLiter(false, _) => return stats
      case _                   =>
    }

    // Updates variables in kvs to phi variables
    map
      .map(getPhiVar)
      .foreach(tup => {
        val (v, i) = tup
        val (_, y, _) = dict(v)
        kvs += ((y.toString + v, i))
      })

    currSTable = currSTable.getNextScopeSSA
    val ssa =
      transformStat(s) ++ mapList.map(x => updatePhiVar(x, Undefined_Map))
    currSTable = currSTable.getPrevScope

    // Remove phi var from kvs so if called upon later, ident is used
    mapList.foreach((x: (String, Int)) => {
      val (s, _) = x
      val (_, y, _) = dict(s)
      kvs -= y.toString + s
    })

    stats += While(e, Seq(ssa.toList))
    stats
  }

  /* Function used to retrieve the latest value of a heap variable in kvs.
     If not present in kvs then it returns the most recent ident associated
     with the given heap variable. */
  private def getLatestHeapExpr(elemName: String): Expr = {
    val (_, y, _) = dict(elemName)
    val elemIdent = y.toString + elemName
    toExpr(kvs.getOrElse(elemIdent, Ident(elemIdent, Undefined_Pos)))
  }

  /* Updates the elements of an arrayLiter using the updated arrayElem values
     from the kvs. */
  private def updateArrayLiter(
      elems: List[Expr],
      pos: (Int, Int),
      arrName: String
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
      kvs += ((addToDict(arrName), updatedArr))
    }
    updatedArr
  }

  /* Updates the elements of a Newpair using the updated fst and snd values
     from the kvs. */
  private def updateNewpair(pairName: String, oldPair: Newpair): AssignRHS = {
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

  private def transExpArray(e: Expr, pf: (Expr => Stat)): ListBuffer[Stat] = {
    val buf = ListBuffer.empty[Stat]
    e match {
      case id @ Ident(s, _) =>
        var id2 @ Ident(x, _) = updateIdent(id)
        val rhs = kvs.getOrElse(x, id2)
        rhs match {
          // Not array, transformExpr as usual
          case _: Expr => buf += pf(toExpr(rhs))
          // Heap variable has not been defined in current ast then add to list
          // of statements
          case r =>
            val rhsUpdated = r match {
              case ArrayLiter(Some(es), pos) => updateArrayLiter(es, pos, s)
              // Empty arrayLiter case, nothing to update
              case ArrayLiter(None, pos)          => ArrayLiter(None, pos)
              case pair: Newpair                  => updateNewpair(s, pair)
              case call @ Call(funcId, args, pos) => call
              case _                              => ???
            }
            // Get latest ident as it may be updated
            id2 = updateIdent(id)
            val t = currSTable.lookupAllType(id)
            stackSize += getBaseTypeSize(t)
            buf += EqIdent(t, id2, rhsUpdated)
            buf += pf(id2)
        }
      // Not ident so not heap variable, transformExpr as usual
      case _ => buf += pf(transformExpr(e))
    }
    buf
  }

  /* Partial function to retrieve runtime error from inside a statement. */
  private def getRuntimeError: PartialFunction[Stat, Stat] = {
    case EqIdent(_, _, err: RuntimeErr) => err
    case EqAssign(_, err: RuntimeErr)   => err
    case Free(err: RuntimeErr)          => err
    case Return(err: RuntimeErr)        => err
    case Exit(err: RuntimeErr)          => err
    case Print(err: RuntimeErr)         => err
    case PrintLn(err: RuntimeErr)       => err
    case If(err: RuntimeErr, _, _)      => err
    case While(err: RuntimeErr, _)      => err
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
          case s if getRuntimeError.isDefinedAt(s) =>
            stop = true
            curStat = ListBuffer(getRuntimeError(s))
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

  /* Transforms a given statement S into SSA form. */
  private def transformStat(s: Stat): ListBuffer[Stat] = {
    val buf = ListBuffer.empty[Stat]
    s match {
      case EqIdent(t, id, r) =>
        val (v, rhs) = addToHashMap(id, r)
        scopeRedefine(id)
        deadCodeElimination(rhs, t, v, buf)
      case EqAssign(id: Ident, r) =>
        val (v, rhs) = addToHashMap(id, r)
        deadCodeElimination(rhs, id.getType(currSTable), v, buf)
      case EqAssign(ae @ ArrayElem(Ident(str, pos), es, _), r) =>
        // foldIntOps(e) > es.length -> exit 255
        // TODO: Array out of bounds check
        val (v, rhs) = addToHashMap(Ident(arrayElemIdentifier(str, es), pos), r)
        deadCodeElimination(rhs, ae.getType(currSTable), v, buf)
      case EqAssign(fst @ Fst(Ident(str, pos), _), r) =>
        val (v, rhs) =
          addToHashMap(Ident(pairElemIdentifier(str, Is_Fst), pos), r)
        deadCodeElimination(rhs, fst.getType(currSTable), v, buf)
      case EqAssign(snd @ Snd(Ident(str, pos), _), r) =>
        val (v, rhs) =
          addToHashMap(Ident(pairElemIdentifier(str, Not_Fst), pos), r)
        deadCodeElimination(rhs, snd.getType(currSTable), v, buf)
      // TODO: EqAssign, deref ptr case
      case Read(lhs)        => transformRead(lhs)
      case Free(e)          => transExpArray(e, Free)
      case Return(e)        => transExpArray(e, Return)
      case Exit(e)          => buf += Exit(transformExpr(e))
      case Print(e)         => transExpArray(e, Print)
      case PrintLn(e)       => transExpArray(e, PrintLn)
      case If(cond, s1, s2) => transformIf(cond, s1, s2)
      case Seq(statList)    => transformSeqStat(statList)
      case Skip             => buf += Skip
      case While(cond, s)   => transformWhile(cond, s)
      case Begin(s) =>
        currSTable = currSTable.getNextScopeSSA
        val transformedS = transformStat(s)
        currSTable = currSTable.getPrevScope
        dict = dict.map(leavingScope)
        transformedS
      case s => buf += s
    }
  }

  /* Update the scope assignment counter to 1 if variable is redeclared
     (eqIdent) in scope  */
  def scopeRedefine(id: Ident): Unit = {
    val Ident(s, _) = id
    var (x, y, z) = dict(s)
    dict += ((s, (x, y, Initial_Id_Num + 1)))
  }

  /* Update the current var number to correct value when leaving scope.
     Reset the scope counter to 0 when leaving scope. */
  def leavingScope(kv: (String, (Int, Int, Int))): (String, (Int, Int, Int)) = {
    val (s, (x, y, z)) = kv
    (s, (x, y - z, Initial_Id_Num))
  }

  /* Transforms a given function F into SSA form. */
  def transformFunc(f: Func): (Func, StackSize) = {
    val Func(t, id, params, s) = f
    val newParams =
      params.map(pList =>
        pList.map(i => {
          val Ident(v, pos) = i
          Ident(addToDict(v), pos)
        })
      )
    currSTable = currSTable.getNextScopeSSA
    val stat = transformStat(s)
    currSTable = currSTable.getPrevScope
    val funcStackSize = stackSize
    stackSize = 0
    (Func(t, id, newParams, Seq(stat.toList)), funcStackSize)
  }

  /* Transforms a given program AST into SSA form. */
  def toSSA(ast: Program): (Program, List[StackSize]) = {
    val Program(fs, s) = ast
    val (funcs, funcStackSizes) = fs.map(transformFunc).unzip
    val p = Program(funcs, Seq(transformStat(s).toList))
    val stackSizes = funcStackSizes :+ stackSize
    (p, stackSizes)
  }
}
