package frontend

import Rules._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import backend.CodeGenerator.getExprType
import frontend.Semantics.SymbolTable
import ConstantFolding.{foldExpr, foldIntOps}

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

  /* Transforms a read function to SSA */
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
      case Read(lhs)  => buf ++= transformRead(lhs)
      case Free(e)    => buf ++= transExpArray(e, Free)
      case Return(e)  => buf ++= transExpArray(e, Return)
      case Exit(e)    => buf += Exit(transformExpr(e))
      case Print(e)   => buf ++= transExpArray(e, Print)
      case PrintLn(e) => buf ++= transExpArray(e, PrintLn)
      // case If(cond, s1, s2) =>
      //   val BoolLiter(b, _) = foldExpr(transformExpr(cond))
      //   if (b) buf ++= transformStat(s1) else buf ++= transformStat(s2)
      case Seq(statList) => statList.flatMap(transformStat).to(ListBuffer)
      case Skip          => buf += Skip
      case s             => buf += s
    }
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
