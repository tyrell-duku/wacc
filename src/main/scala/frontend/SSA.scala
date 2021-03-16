package frontend

import Rules._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import backend.CodeGenerator.getExprType
import frontend.Semantics.SymbolTable

case class SSA(sTable: SymbolTable) {
  // <Identifier's name, (LHS unique identifier, RHS unique identifier)>
  private var dict = new HashMap[String, (Int, Int)]
  private var kvs = new HashMap[String, AssignRHS]
  val INITIAL_LHS = 0
  val INITIAL_RHS = 0

  /* If the hashmap doesn't contain the identifier STR, then it is added to
     the hashmap with initial values (1, 1). If the hashmap contains the
     identifier STR, then the values are updated for unique identifying. */
  private def addToHashMap(id: Ident, rhs: AssignRHS): Ident = {
    val Ident(str, pos) = id
    val v = addToDict(str)
    rhs match {
      case e: Expr => kvs += ((v, transformExpr(e)))
      case arr: ArrayLiter =>
        kvs += ((v, arr))
        arr match {
          case ArrayLiter(Some(es), _) =>
            for (i <- es.indices) {
              val aeName = addToDict(str + "-" + i)
              kvs += ((aeName, transformExpr(es(i))))
            }
          case _ =>
        }
      case newpair: Newpair =>
        kvs += ((v, newpair))
        val Newpair(fst, snd, _) = newpair
        val fstName = addToDict(pairElemIdentifier(str, true))
        kvs += ((fstName, transformExpr(fst)))
        val sndName = addToDict(pairElemIdentifier(str, false))
        kvs += ((sndName, transformExpr(snd)))
      case Fst(Ident(s, _), _) =>
        kvs += ((v, getLatestHeapExpr(s + "-fst")))
      case Snd(Ident(s, _), _) =>
        kvs += ((v, getLatestHeapExpr(s + "-snd")))
      case _ =>

    }
    Ident(v, pos)
  }

  /* Add the string s to dict hashmap. If already present then variable counter
     is incremented by 1 and updated in dict. The string for the next unique
     identifier is returned. */
  private def addToDict(s: String): String = {
    var (x, y) = dict.getOrElse(s, (INITIAL_LHS, INITIAL_RHS))
    x += 1
    y += 1
    dict += ((s, (x, y)))
    x.toString + s
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
      toExpr(kvs.getOrElse(x, id2))
    case ArrayElem(id @ Ident(s, _), es, pos) =>
      val arrayElemName = arrayElemIdentifier(s, es)
      val (_, y) = dict(arrayElemName)
      toExpr(
        kvs.getOrElse(
          y.toString + arrayElemName,
          Ident(y.toString + arrayElemName, null)
        )
      )
    case x: IntLiter  => x
    case b: BoolLiter => b
    case c: CharLiter => c
    case s: StrLiter  => s
    case p: PairLiter => p
    // All operators
    case _ => e.map(transformExpr)
  }

  /* Transforms an expression E for the condition of a while loop. */
  private def transformExpr(e: Expr, map: Map[String, Int]): Expr = e match {
    case sizeof: SizeOf => sizeof
    case id @ Ident(s, p) =>
      val v = map.getOrElse(s, 0)
      // If ident not present in map, it is not changed in while loop so,
      // transformExpr as usual
      if (v == 0) {
        transformExpr(id)
      } else {
        // Update id to use phi var
        val (_, y) = dict(s)
        Ident((v + y).toString + s, p)
      }
    case ae: ArrayElem => transformExpr(ae)
    case x: IntLiter   => x
    case b: BoolLiter  => b
    case c: CharLiter  => c
    case s: StrLiter   => s
    case p: PairLiter  => p
    // All operators
    case _ =>
      e.map(exp => transformExpr(exp, map))
  }

  private def deadCodeElimination(
      rhs: AssignRHS,
      stat: Stat,
      buf: ListBuffer[Stat]
  ): ListBuffer[Stat] =
    rhs match {
      case _: Expr | _: ArrayLiter | _: Newpair | _: PairElem => buf
      case _                                                  => buf += stat
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
        val fstName = pairElemIdentifier(s, true)
        e = getLatestHeapExpr(fstName)
        fstName
      case Snd(id @ Ident(s, _), _) =>
        val sndName = pairElemIdentifier(s, false)
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
        val v = Ident(addToDict(varName), null)
        ListBuffer(EqIdent(getExprType(e), v, e), Read(v))
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
        incrementMap(pairElemIdentifier(s, true), map, mapScope)
      case EqAssign(Snd(Ident(s, _), _), _) =>
        incrementMap(pairElemIdentifier(s, false), map, mapScope)
      case Read(Ident(s, _)) =>
        incrementMap(s, map, mapScope)
      case Read(ArrayElem(Ident(s, _), elems, _)) =>
        incrementMap(arrayElemIdentifier(s, elems), map, mapScope)
      case Read(Fst(Ident(s, _), _)) =>
        incrementMap(pairElemIdentifier(s, true), map, mapScope)
      case Read(Snd(Ident(s, _), _)) =>
        incrementMap(pairElemIdentifier(s, false), map, mapScope)
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
      if (mapScope != null) {
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
    val (_, y) = dict(s)
    val varName = y.toString + s
    // Get previously defined value from kvs
    val rhs = kvs.getOrElse(varName, Ident(varName, null))
    val originalType = Ident(s, null).getType(sTable)
    EqIdent(originalType, Ident((n + y).toString + s, null), rhs)
  }

  /* Updates the PHI variable, given the map of the condition branch */
  private def updatePhiVar(x: (String, Int), map: Map[String, Int]): Stat = {
    val (s, _) = x
    val (_, y) = dict(s)
    val varName = y.toString + s
    val rhs = kvs.getOrElse(varName, Ident(varName, null))
    if (map != null) {
      EqAssign(Ident((map.getOrElse(s, 0) + y).toString + s, null), rhs)
    } else {
      EqAssign(Ident(y.toString + s, null), rhs)
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
    val ssa1 =
      transformStat(s1) ++ mapIf.toList.map(x => updatePhiVar(x, mapElse))
    val ssa2 =
      transformStat(s2) ++ mapElse.toList.map(x => updatePhiVar(x, null))
    val ifStat = If(e, pruneIf(ssa1), pruneIf(ssa2))

    // Remove phi var from kvs so if called upon later, ident is used
    mapList.foreach((x: (String, Int)) => {
      val (_, y) = dict(x._1)
      kvs -= y.toString + x._1
    })

    ifStat match {
      case If(_, Skip, Skip) => stats
      case _                 => stats += ifStat
    }
  }

  private def transformWhile(cond: Expr, s: Stat): ListBuffer[Stat] = {
    val stats = ListBuffer.empty[Stat]
    // Number of re-assignments of pre-declared vars within WHILE
    val map = Map.empty[String, Int]
    // Count var re-assignments into map
    countVariables(s, map, null)

    // Create the PHI variables if necessary
    val mapList = map.toList
    stats ++= mapList.map(createPhiVar)

    // Transform the while condition, updating with the phi vars if necessary
    // Transform the inner of the while, updating phi vars if necessary
    val e = transformExpr(cond, map)
    val ssa = transformStat(s) ++ map.toList.map(x => updatePhiVar(x, null))

    // Remove phi var from kvs so if called upon later, ident is used
    mapList.foreach((x: (String, Int)) => {
      val (_, y) = dict(x._1)
      kvs -= y.toString + x._1
    })
    stats += While(e, Seq(ssa.toList))
    stats
  }

  /* Function used to retrieve the latest value of a heap variable in kvs.
     If not present in kvs then it returns the most recent ident associated
     with the given heap variable. */
  private def getLatestHeapExpr(elemName: String): Expr = {
    val (_, y) = dict(elemName)
    val elemIdent = y.toString + elemName
    toExpr(kvs.getOrElse(elemIdent, Ident(elemIdent, null)))
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
          case rhs =>
            val rhsUpdated = rhs match {
              case ArrayLiter(Some(es), pos) => updateArrayLiter(es, pos, s)
              // Empty arrayLiter case, nothing to update
              case ArrayLiter(None, pos) => ArrayLiter(None, pos)
              case pair: Newpair         => updateNewpair(s, pair)
              case _                     => ???
            }
            // Get latest ident as it may be updated
            id2 = updateIdent(id)
            val t = sTable.lookupAllType(id)
            buf += EqIdent(t, id2, rhsUpdated)
            buf += pf(id2)
        }
      // Not ident so not heap variable, transformExpr as usual
      case _ => buf += pf(transformExpr(e))
    }
    buf
  }

  /* Transforms a given statement S into SSA form. */
  private def transformStat(s: Stat): ListBuffer[Stat] = {
    val buf = ListBuffer.empty[Stat]
    s match {
      case EqIdent(t, id, r) =>
        val v = addToHashMap(id, r)
        deadCodeElimination(r, EqIdent(t, v, r), buf)
      case EqAssign(id: Ident, r) =>
        val v = addToHashMap(id, r)
        deadCodeElimination(r, EqAssign(v, r), buf)
      case EqAssign(ArrayElem(Ident(str, pos), es, _), r) =>
        // foldIntOps(e) > es.length -> exit 255
        // TODO: Array out of bounds check
        val v = addToHashMap(Ident(arrayElemIdentifier(str, es), pos), r)
        deadCodeElimination(r, EqAssign(v, r), buf)
      case EqAssign(Fst(Ident(str, pos), _), r) =>
        val v = addToHashMap(Ident(pairElemIdentifier(str, true), pos), r)
        deadCodeElimination(r, EqAssign(v, r), buf)
      case EqAssign(Snd(Ident(str, pos), _), r) =>
        val v = addToHashMap(Ident(pairElemIdentifier(str, false), pos), r)
        deadCodeElimination(r, EqAssign(v, r), buf)
      //TODO: EqAssign, deref ptr case
      case Read(lhs)        => buf ++= transformRead(lhs)
      case Free(e)          => buf ++= transExpArray(e, Free)
      case Return(e)        => buf ++= transExpArray(e, Return)
      case Exit(e)          => buf += Exit(transformExpr(e))
      case Print(e)         => buf ++= transExpArray(e, Print)
      case PrintLn(e)       => buf ++= transExpArray(e, PrintLn)
      case If(cond, s1, s2) => buf ++= transformIf(cond, s1, s2)
      case Seq(statList)    => statList.flatMap(transformStat).to(ListBuffer)
      case Skip             => buf += Skip
      case While(cond, s)   => buf ++= transformWhile(cond, s)
      case s                => buf += s
    }
  }

  /* Transforms a given function F into SSA form. */
  def transformFunc(f: Func): Func = {
    val Func(t, id, params, s) = f
    Func(
      t,
      id,
      params.map(pList => pList.map(i => addToHashMap(i, null))),
      Seq(transformStat(s).toList)
    )
  }

  /* Transforms a given program AST into SSA form. */
  def toSSA(ast: Program): Program = {
    val Program(fs, s) = ast
    Program(fs.map(f => transformFunc(f)), Seq(transformStat(s).toList))
  }
}
