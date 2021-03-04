package backend

import InstructionSet._
import frontend.Rules._
import scala.collection.mutable.ListBuffer
import PrintInstrs._
import ReadInstructions._
import RuntimeErrors._

object CodeGenerator {
  final val allRegs: ListBuffer[Reg] =
    ListBuffer(R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12)

  var freeRegs = allRegs
  final val resultReg: Reg = R0
  freeRegs -= resultReg
  freeRegs -= R1
  freeRegs -= R2
  freeRegs -= R3

  var varTable = Map.empty[Ident, (Int, Type)]
  var currentSP = 0
  private var currentLabel = Label("main")

  private var labelCounter = 0

  private val dataTable = new DataTable
  private val userFuncTable = new FuncTable
  private val funcTable = new FuncTable

  private val INT_SIZE = 4
  private val CHAR_SIZE = 1
  private val BOOL_SIZE = 1
  private val STR_SIZE = 4
  private val ARRAY_SIZE = 4
  private val PAIR_SIZE = 4
  private val MAX_INT_IMM = 1024

  private def saveRegs(
      regsNotInUse: ListBuffer[Reg]
  ): Instruction = {
    val regsToPush = allRegs.filter(r => !regsNotInUse.contains(r))
    Push(regsToPush)
  }

  private def restoreRegs(
      regsNotInUse: ListBuffer[Reg]
  ): Instruction = {
    val regsToPush = allRegs.filter(r => !regsNotInUse.contains(r)).reverse
    new Pop(regsToPush)
  }

  private def addRuntimeError(err: RuntimeError): Unit = {
    funcTable.addEntry(throwRuntimeError())
    funcTable.addEntry(stringPrintInstrs)
    dataTable.addDataEntryWithLabel("msg_string", "%.*s\\0")
    err match {
      case ArrayBounds =>
        funcTable.addEntry(
          checkArrayBounds(
            dataTable.addDataEntryWithLabel(
              "msg_neg_index",
              "ArrayIndexOutOfBoundsError: negative index\\n\\0"
            ),
            dataTable.addDataEntryWithLabel(
              "msg_index_too_large",
              "ArrayIndexOutOfBoundsError: index too large\\n\\0"
            )
          )
        )
      case DivideByZero =>
        funcTable.addEntry(
          checkDivideByZero(
            dataTable.addDataEntryWithLabel(
              "msg_divide_by_zero",
              "DivideByZeroError: divide or modulo by zero\\n\\0"
            )
          )
        )
      case Overflow =>
        funcTable.addEntry(
          throwOverflowError(
            dataTable.addDataEntryWithLabel(
              "msg_overflow",
              "OverflowError: the result is too small/large to store in a 4-byte signed-integer.\\n"
            )
          )
        )
      case FreePair =>
        funcTable.addEntry(
          freePair(
            dataTable.addDataEntryWithLabel(
              "msg_null_reference",
              "NullReferenceError: dereference a null reference\\n\\0"
            )
          )
        )
      case NullPointer =>
        funcTable.addEntry(
          checkNullPointer(
            dataTable.addDataEntryWithLabel(
              "msg_null_reference",
              "NullReferenceError: dereference a null reference\\n\\0"
            )
          )
        )
    }
  }

  def transProg(
      prog: Program
  ): (List[Data], List[(Label, List[Instruction])]) = {
    val Program(funcs, stat) = prog
    for (f <- funcs) {
      transFunc(f)
    }
    currentLabel = Label("main")
    val instructions = transStat(stat, ListBuffer(Push(ListBuffer(LR))))
    var toAdd = addSP() ++ ListBuffer(
      Ldr(resultReg, ImmMem(0)),
      Pop(ListBuffer(PC)),
      Ltorg
    )
    instructions ++= toAdd
    userFuncTable.addEntry(currentLabel, instructions.toList)
    val funcList = userFuncTable.table ++ funcTable.table

    (dataTable.table.toList, funcList.toList)
  }

  private def transFunc(func: Func): Unit = {
    val Func(t, Ident(id, _), ps, s) = func
    currentLabel = Label("f_" + id)
    ps match {
      case None =>
      case Some(paramList) =>
        val ParamList(plist) = paramList
        var currSp = 4
        var prevSize = 0
        for (param <- plist) {
          val Param(t, i) = param
          currSp += prevSize
          prevSize = getBaseTypeSize(t)
          varTable += (i -> (-currSp, t))
        }
    }

    val instrs = transStat(s, ListBuffer(Push(ListBuffer(LR))))

    userFuncTable.addEntry(currentLabel, instrs.toList)
    varTable = Map.empty[Ident, (Int, Type)]
  }

  /* Translates read identifiers to the internal representation.
     Only types int and char are semantically valid. */
  private def transReadIdent(ident: Ident): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    val freeReg = getFreeReg()
    val (spIndex, identType) = varTable(ident)
    val spOffset = currentSP - spIndex
    instructions += InstructionSet.Add(
      freeReg,
      SP,
      ImmInt(spOffset)
    )
    // variable must be in R0 for the branch
    instructions += Mov(R0, freeReg)
    addUnusedReg(freeReg)
    // pattern matching for which read label to use
    identType match {
      case CharT =>
        instructions += BranchLink(Label("p_read_char"))
        funcTable.addEntry(charRead(dataTable.addDataEntry(" %c\\0")))
      case IntT =>
        instructions += BranchLink(Label("p_read_int"))
        funcTable.addEntry(intRead(dataTable.addDataEntry("%d\\0")))
      // Semantically incorrect
      case _ => ListBuffer.empty[Instruction]
    }
    instructions
  }

  /* Translates read array-elems to the internal representation. */
  def transReadArrayElem(ae: ArrayElem): ListBuffer[Instruction] = {
    val ArrayElem(ident, exprs, _) = ae
    val instructions = ListBuffer.empty[Instruction]
    val resReg = getFreeReg()

    // Handles nested arrays
    val (_, instrs) = transArrayElem(ident, exprs, resReg)
    instructions ++= instrs
    // value must be in R0 for branch
    instructions += Mov(R0, resReg)
    addUnusedReg(resReg)

    // Gets base type of the arrayElem
    val t = getExprType(ae)
    t match {
      case CharT =>
        instructions += BranchLink(Label("p_read_char"))
        funcTable.addEntry(charRead(dataTable.addDataEntry(" %c\\0")))
      case IntT =>
        instructions += BranchLink(Label("p_read_int"))
        funcTable.addEntry(charRead(dataTable.addDataEntry("%d\\0")))
      // Semantically incorrect
      case _ =>
    }
    instructions
  }

  /* Translates read statements to the internal representation. */
  private def transRead(lhs: AssignLHS): ListBuffer[Instruction] = {
    lhs match {
      case ident: Ident  => transReadIdent(ident)
      case ae: ArrayElem => transReadArrayElem(ae)
      case _             => ListBuffer.empty[Instruction]
    }
  }

  private def transReturn(e: Expr): ListBuffer[Instruction] = {
    val reg = getFreeReg()
    val instrs = transExp(e, reg) ++ ListBuffer(
      Mov(resultReg, reg),
      Add(SP, SP, ImmInt(currentSP)),
      Pop(ListBuffer(PC)),
      Pop(ListBuffer(PC)),
      Ltorg
    )
    currentSP = 0
    addUnusedReg(reg)
    instrs
  }

  private def transStat(
      stat: Stat,
      instructions: ListBuffer[Instruction]
  ): ListBuffer[Instruction] = {
    stat match {
      case EqIdent(t, i, r) => instructions ++= transEqIdent(t, i, r)
      case EqAssign(l, r)   => instructions ++= transEqAssign(l, r)
      case Read(lhs)        => instructions ++= transRead(lhs)
      // case Free(e)          => instructions ++= ListBuffer.empty[Instruction]
      case Return(e)        => instructions ++= transReturn(e)
      case Exit(e)          => instructions ++= transExit(e)
      case Print(e)         => instructions ++= transPrint(e, false)
      case PrintLn(e)       => instructions ++= transPrint(e, true)
      case If(cond, s1, s2) => transIf(cond, s1, s2, instructions)
      case While(cond, s)   => transWhile(cond, s, instructions)
      case Seq(statList) =>
        var nextInstructions = instructions
        for (s <- statList) {
          nextInstructions = transStat(s, nextInstructions)
        }
        nextInstructions
      case _ => instructions
    }
  }

  private def getInnerType(t: Type) = t match {
    case ArrayT(inner) => inner
    // invalid case, will never enter
    case _ => null
  }

  private def getExprType(e: Expr): Type = {
    e match {
      case IntLiter(_, _)  => IntT
      case BoolLiter(_, _) => BoolT
      case CharLiter(_, _) => CharT
      case StrLiter(_, _)  => StringT
      case PairLiter(_)    => Pair(null, null)
      case id: Ident =>
        val (_, t) = varTable.apply(id)
        t
      case ArrayElem(id, es, _) =>
        var (_, t) = varTable.apply(id)
        for (_ <- es) {
          t = getInnerType(t)
        }
        t
      case Not(_, _)      => BoolT
      case Negation(_, _) => IntT
      case Len(_, _)      => IntT
      case Ord(_, _)      => IntT
      case Chr(_, _)      => CharT
      case _: ArithOps    => IntT
      case _: ComparOps   => BoolT
      case _: EqOps       => BoolT
      case _: LogicalOps  => BoolT
    }
  }

  private def transPrint(
      e: Expr,
      isNewLine: Boolean
  ): ListBuffer[Instruction] = {
    val t = getExprType(e)
    val freeReg = getFreeReg()
    val instrs = transExp(e, freeReg)
    instrs += Mov(resultReg, freeReg)
    t match {
      case CharT =>
        instrs += BranchLink(Label("putchar"))
      case IntT =>
        dataTable.addDataEntryWithLabel("msg_int", "%d\\0")
        instrs += BranchLink(Label("p_print_int"))
        funcTable.addEntry(intPrintInstrs)
      case BoolT =>
        dataTable.addDataEntryWithLabel("msg_true", "true\\0")
        dataTable.addDataEntryWithLabel("msg_false", "false\\0")
        instrs += BranchLink(Label("p_print_bool"))
        funcTable.addEntry(boolPrintInstrs)
      case StringT =>
        dataTable.addDataEntryWithLabel("msg_string", "%.*s\\0")
        instrs += BranchLink(Label("p_print_string"))
        funcTable.addEntry(stringPrintInstrs)
      case Pair(null, null) =>
        dataTable.addDataEntryWithLabel("msg_reference", "%p\\0")
        instrs += BranchLink(Label("p_print_reference"))
        funcTable.addEntry(referencePrintInstrs)
      case ArrayT(CharT) =>
        dataTable.addDataEntryWithLabel("msg_string", "%.*s\\0")
        instrs += BranchLink(Label("p_print_string"))
        funcTable.addEntry(stringPrintInstrs)
      case ArrayT(_) =>
        dataTable.addDataEntryWithLabel("msg_reference", "%p\\0")
        instrs += BranchLink(Label("p_print_reference"))
        funcTable.addEntry(referencePrintInstrs)

      case _ =>
    }
    if (isNewLine) {
      instrs += BranchLink(Label("p_print_ln"))
      dataTable.addDataEntryWithLabel("msg_new_line", "\\0")
      funcTable.addEntry(newLinePrintInstrs)
    }
    addUnusedReg(freeReg)
    instrs
  }

  private def assignLabel(): Label = {
    val nextLabel = Label("L" + labelCounter)
    labelCounter += 1
    nextLabel
  }

  private def transIf(
      cond: Expr,
      s1: Stat,
      s2: Stat,
      curInstrs: ListBuffer[Instruction]
  ): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    val reg = getFreeReg()
    curInstrs ++= transExp(cond, reg)
    // check if condition is false
    curInstrs += Cmp(reg, ImmInt(0))
    addUnusedReg(reg)
    val elseBranch = assignLabel()
    curInstrs += BranchEq(elseBranch)
    // statement if the condition was true
    curInstrs ++= transStat(s1, ListBuffer.empty[Instruction])

    val afterLabel = assignLabel()
    curInstrs += Branch(afterLabel)

    userFuncTable.addEntry(currentLabel, curInstrs.toList)
    currentLabel = elseBranch
    userFuncTable.addEntry(
      currentLabel,
      transStat(s2, ListBuffer.empty[Instruction]).toList
    )
    currentLabel = afterLabel

    instructions
  }

  private def transWhile(
      cond: Expr,
      s: Stat,
      curInstrs: ListBuffer[Instruction]
  ): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    val afterLabel = assignLabel()
    curInstrs += Branch(afterLabel)
    userFuncTable.addEntry(currentLabel, curInstrs.toList)

    val insideWhile = assignLabel()
    currentLabel = insideWhile
    val transInnerWhile = transStat(s, ListBuffer.empty[Instruction]).toList
    userFuncTable.addEntry(currentLabel, transInnerWhile)

    currentLabel = afterLabel

    val reg = getFreeReg()
    instructions ++= transExp(cond, reg)
    // check if condition is true
    instructions += Cmp(reg, ImmInt(1))
    addUnusedReg(reg)
    instructions += BranchEq(insideWhile)
  }

  private def transExit(
      e: Expr
  ): ListBuffer[Instruction] = {
    val freeReg = getFreeReg()
    val instructions = transExp(e, freeReg)
    instructions ++= ListBuffer[Instruction](
      Mov(R0, freeReg),
      BranchLink(Label("exit"))
    )
    addUnusedReg(freeReg)
    instructions
  }

  private def transEqIdent(
      t: Type,
      id: Ident,
      aRHS: AssignRHS
  ): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    currentSP += getBaseTypeSize(t)
    varTable += (id -> (currentSP, t))

    instructions += InstructionSet.Sub(SP, SP, ImmInt(getBaseTypeSize(t)))

    val freeReg = getFreeReg()
    val (isByte, instrs) = assignRHS(t, aRHS, freeReg)
    instructions ++= instrs
    if (isByte) {
      instructions += StrB(freeReg, RegAdd(SP))
    } else {
      instructions += Str(freeReg, RegAdd(SP))
    }

    addUnusedReg(freeReg)
    instructions
  }

  private def addSP(): ListBuffer[Instruction] = {
    var curSp = currentSP
    val instrs = ListBuffer.empty[Instruction]
    while (curSp > MAX_INT_IMM) {
      curSp -= MAX_INT_IMM
      instrs += InstructionSet.Add(SP, SP, ImmInt(MAX_INT_IMM))
    }
    instrs += InstructionSet.Add(SP, SP, ImmInt(curSp))
    instrs
  }

  private def loadArgs(
      args: Option[ArgList] = None,
      reg: Reg
  ): (ListBuffer[Instruction], Int) = {
    var instrs = ListBuffer.empty[Instruction]
    var totalOff = 0
    args match {
      case None =>
      case Some(argList) =>
        val ArgList(aList) = argList
        for (e <- aList) {
          val t = getExprType(e)
          if (t == CharT || t == BoolT) {
            instrs += StrBOffsetIndex(reg, SP, -getBaseTypeSize(t))
          } else {
            instrs += StrOffsetIndex(reg, SP, -getBaseTypeSize(t))
          }
          totalOff += getBaseTypeSize(t)
          instrs ++= transExp(e, reg)
        }
    }
    (instrs.reverse, totalOff)
  }

  private def assignRHSPair(
      p: Type,
      rhs: AssignRHS,
      freeReg: Reg
  ): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    val nextFreeReg = getFreeReg()
    val Pair(fstType, sndType) = p
    rhs match {
      case Newpair(fst, snd, _) =>
        // Every pair requires 8 bytes
        instructions += Ldr(R0, ImmMem(2 * PAIR_SIZE))
        instructions += BranchLink(Label("malloc"))
        instructions += Mov(freeReg, R0)
        instructions ++= transExp(fst, nextFreeReg)
        // Size of fst rhs
        instructions += Ldr(R0, ImmMem(getPairElemTypeSize(fstType)))
        instructions += BranchLink(Label("malloc"))
        if (pairIsByte(p, true)) {
          instructions += StrB(nextFreeReg, RegAdd(R0))
        } else {
          instructions += Str(nextFreeReg, RegAdd(R0))
        }
        instructions += Str(R0, RegAdd(freeReg))
        instructions ++= transExp(snd, nextFreeReg)
        // Size of snd rhs
        instructions += Ldr(R0, ImmMem(getPairElemTypeSize(sndType)))
        instructions += BranchLink(Label("malloc"))
        if (pairIsByte(p, false)) {
          instructions += StrB(nextFreeReg, RegAdd(R0))
        } else {
          instructions += Str(nextFreeReg, RegAdd(R0))
        }
        instructions += Str(R0, RegisterOffset(freeReg, PAIR_SIZE))
      case ident: Ident =>
        val (spIndex, _) = varTable(ident)
        instructions += Ldr(
          freeReg,
          RegisterOffset(SP, currentSP - spIndex)
        )
      // Semantically incorrect
      case _ => instructions
    }
    instructions
  }

  private def isByte(t: Type): Boolean = {
    t == BoolT || t == CharT
  }

  private def pairIsByte(idType: Type, fst: Boolean): Boolean = {
    idType match {
      case Pair(PairElemT(x), PairElemT(y)) => if (fst) isByte(x) else isByte(y)
      case _                                => false
    }
  }

  private def transPairElem(
      id: Ident,
      fst: Boolean,
      freeReg: Reg
  ): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    val (spOffset, idType) = varTable(id)
    val spIndex = currentSP - spOffset
    instructions += Ldr(freeReg, RegisterOffset(SP, spIndex))
    // Value must be in R0 for branch
    instructions += Mov(R0, freeReg)
    // Runtime error
    instructions += BranchLink(Label("p_check_null_pointer"))
    addRuntimeError(NullPointer)
    if (fst) {
      // For Fst
      instructions += Ldr(freeReg, RegAdd(freeReg))
    } else {
      // For Snd
      instructions += Ldr(freeReg, RegisterOffset(freeReg, PAIR_SIZE))
    }
    instructions
  }

  private def loadPairElem(
      id: Ident,
      freeReg: Reg,
      isFst: Boolean
  ): Instruction = {
    val (_, idType) = varTable(id)
    if (pairIsByte(idType, isFst)) {
      LdrSB(freeReg, RegAdd(freeReg))
    } else {
      Ldr(freeReg, RegAdd(freeReg))
    }
  }

  private def assignRHS(
      t: Type,
      aRHS: AssignRHS,
      freeReg: Reg
  ): (Boolean, ListBuffer[Instruction]) = {
    val instructions = ListBuffer.empty[Instruction]
    t match {
      case IntT | CharT | BoolT | StringT =>
        aRHS match {
          case ex: Expr => instructions ++= transExp(ex, freeReg)
          // PairElem
          case Fst(id: Ident, _) =>
            instructions ++= transPairElem(id, true, freeReg)
            instructions += loadPairElem(id, freeReg, true)
          case Snd(id: Ident, _) =>
            instructions ++= transPairElem(id, false, freeReg)
            instructions += loadPairElem(id, freeReg, false)
          case Call(id, args, _) =>
            val Ident(i, _) = id
            val (argInstrs, toAdd) = loadArgs(args, freeReg)
            instructions ++= argInstrs ++ ListBuffer(
              BranchLink(Label("f_" + i)),
              Add(SP, SP, ImmInt(toAdd)),
              Mov(freeReg, resultReg)
            )
          case _ => ListBuffer.empty[Instruction]
        }
      case ArrayT(t) =>
        val ArrayLiter(opArr, _) = aRHS
        val arr = opArr match {
          case Some(arr) => arr
          case None      => List.empty[Expr]
        }

        val listSize = arr.size
        val baseTypeSize = getBaseTypeSize(t)
        val sizeToMalloc = 4 + (listSize * baseTypeSize)
        instructions += Ldr(R0, ImmMem(sizeToMalloc))
        instructions += BranchLink(Label("malloc"))
        instructions += Mov(freeReg, R0)
        val nextFreeReg = getFreeReg()

        if (t == CharT || t == BoolT) {
          for (i <- 0 until listSize) {
            instructions ++= transExp(arr(i), nextFreeReg)
            instructions += StrB(nextFreeReg, RegisterOffset(freeReg, i + 4))
          }
        } else {
          for (i <- 0 until listSize) {
            instructions ++= transExp(arr(i), nextFreeReg)
            instructions += Str(
              nextFreeReg,
              RegisterOffset(freeReg, (i + 1) * 4)
            )
          }
        }
        instructions += Ldr(nextFreeReg, ImmMem(listSize))
        instructions += Str(nextFreeReg, RegAdd(freeReg))
        addUnusedReg(nextFreeReg)
      case p: Pair => instructions ++= assignRHSPair(p, aRHS, freeReg)
      case _       => instructions
    }
    (isByte(t), instructions)
  }

  private def transEqAssignPairElem(
      rhs: AssignRHS,
      id: Ident,
      isFst: Boolean,
      freeReg: Reg
  ): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    val (index, t) = varTable.apply(id)
    val pairElemType = if (isFst) {
      t match {
        case Pair(PairElemT(fstType), _) => fstType
        case Pair(PairElemPair, _)       => Pair(null, null)
        // Semantically incorrect
        case _ => null
      }
    } else {
      t match {
        case Pair(_, PairElemT(sndType)) => sndType
        case Pair(_, PairElemPair)       => Pair(null, null)
        // Semantically incorrect
        case _ => null
      }
    }
    val (isByte, instrs) = assignRHS(pairElemType, rhs, freeReg)
    instructions ++= instrs
    val nextReg = getFreeReg()
    instructions ++= transPairElem(id, isFst, nextReg)
    if (isByte) {
      instructions += StrB(freeReg, RegAdd(nextReg))
    } else {
      instructions += Str(freeReg, RegAdd(nextReg))
    }
    addUnusedReg(nextReg)
    instructions
  }

  private def transEqAssign(
      aLHS: AssignLHS,
      aRHS: AssignRHS
  ): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    val freeReg = getFreeReg()
    aLHS match {
      case Fst(id: Ident, _) =>
        instructions ++= transEqAssignPairElem(aRHS, id, true, freeReg)
      case Snd(id: Ident, _) =>
        instructions ++= transEqAssignPairElem(aRHS, id, false, freeReg)
      case id: Ident =>
        val (index, t) = varTable.apply(id)
        val (isByte, instrs) = assignRHS(t, aRHS, freeReg)
        instructions ++= instrs
        val spOffset = currentSP - index

        if (isByte) {
          instructions += StrB(freeReg, RegisterOffset(SP, spOffset))
        } else {
          instructions += Str(freeReg, RegisterOffset(SP, spOffset))
        }

      case ae @ ArrayElem(id, es, _) =>
        val (_, instrs) = assignRHS(getExprType(ae), aRHS, freeReg)
        instructions ++= instrs
        instructions ++= storeArrayElem(id, es, freeReg)
      case _ =>
    }
    addUnusedReg(freeReg)
    instructions
  }

  private def transStrLiter(
      str: StrLiter,
      reg: Reg
  ): ListBuffer[Instruction] = {
    val curLabel = dataTable.addDataEntry(str)
    ListBuffer(Ldr(reg, DataLabel(curLabel)))
  }

  private def loadArrayElem(
      id: Ident,
      es: List[Expr],
      reg: Reg
  ): ListBuffer[Instruction] = {
    val (isByte, instructions) = transArrayElem(id, es, reg)

    if (isByte) {
      instructions += LdrB(reg, RegAdd(reg))
    } else {
      instructions += Ldr(reg, RegAdd(reg))
    }
  }

  private def storeArrayElem(
      id: Ident,
      es: List[Expr],
      reg: Reg
  ): ListBuffer[Instruction] = {
    val freeReg = getFreeReg()
    val (isByte, instructions) = transArrayElem(id, es, freeReg)

    if (isByte) {
      instructions += StrB(reg, RegAdd(freeReg))
    } else {
      instructions += Str(reg, RegAdd(freeReg))
    }

    addUnusedReg(freeReg)
    instructions
  }

  private def transArrayElem(
      id: Ident,
      es: List[Expr],
      reg: Reg
  ): (Boolean, ListBuffer[Instruction]) = {
    var (index, t) = varTable.apply(id)
    val instructions = ListBuffer.empty[Instruction]

    val baseTypeSize = getBaseTypeSize(t)
    val spOffset = currentSP - index
    instructions += InstructionSet.Add(reg, SP, ImmInt(spOffset))
    val nextReg = getFreeReg()
    for (exp <- es) {
      t = getInnerType(t)
      instructions ++= transExp(exp, nextReg)
      instructions += Ldr(reg, RegAdd(reg))

      // Values must be in R0 & R1 for branch
      instructions += Mov(R0, nextReg)
      instructions += Mov(R1, reg)
      instructions += BranchLink(Label("p_check_array_bounds"))
      addRuntimeError(ArrayBounds)

      instructions += Add(reg, reg, ImmInt(INT_SIZE))
      if (t == CharT || t == BoolT) {
        instructions += Add(reg, reg, nextReg)
      } else {
        instructions += Add(reg, reg, LSL(nextReg, ImmInt(2)))
      }
    }
    addUnusedReg(nextReg)
    (t == CharT || t == BoolT, instructions)
  }

  /* Translates unary operator OP to the internal representation. */
  private def transUnOp(op: UnOp, reg: Reg): ListBuffer[Instruction] = {
    op match {
      case Chr(e, _) => transExp(e, reg)
      case Len(id: Ident, _) =>
        val (index, _) = varTable.apply(id)
        ListBuffer(
          Ldr(reg, RegisterOffset(SP, currentSP - index)),
          Ldr(reg, RegAdd(reg))
        )
      case Negation(e, _) =>
        addRuntimeError(Overflow)
        transExp(e, reg) ++= ListBuffer(
          NegInstr(reg, reg),
          BranchLinkVS(Label("p_throw_overflow_error"))
        )
      case Not(e, _) => transExp(e, reg) += Eor(reg, reg, ImmInt(1))
      case Ord(e, _) => transExp(e, reg)
      case _         => ListBuffer.empty[Instruction]
    }
  }

  /* Changes a comparison operator to the Condition equivalent.
     Returns null when not a comparison operator. */
  private def rulesCmpToInstrCmp(cmp: BinOp): Condition = {
    cmp match {
      case GT(lExpr, rExpr, pos)       => backend.GT
      case GTE(lExpr, rExpr, pos)      => backend.GE
      case LT(lExpr, rExpr, pos)       => backend.LT
      case LTE(lExpr, rExpr, pos)      => backend.LE
      case Equal(lExpr, rExpr, pos)    => backend.EQ
      case NotEqual(lExpr, rExpr, pos) => backend.NE
      case _                           => null // Undefined
    }
  }

  /* Translates a comparator operator to the internal representation. */
  private def transCond(op: BinOp, reg: Reg): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]

    val rReg = getFreeReg()
    instructions ++= transExp(op.lExpr, reg)
    instructions ++= transExp(op.rExpr, rReg)
    val cmp = rulesCmpToInstrCmp(op)
    instructions += Cmp(reg, rReg)
    addUnusedReg(rReg)
    instructions += MovCond(cmp, reg, ImmInt(1))
    instructions += MovCond(cmp.oppositeCmp, reg, ImmInt(0))
    instructions
  }

  /* Translates a binary operator to the internal representation. */
  private def transBinOp(op: BinOp, reg: Reg): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    // TODO: determine result register for l & r
    op match {
      case frontend.Rules.Mul(l, r, _) =>
        val rReg = getFreeReg()
        instructions ++= transExp(l, reg)
        instructions ++= transExp(r, rReg)
        // Runtime error check
        instructions += InstructionSet.SMul(reg, rReg, reg, rReg)
        instructions += Cmp(rReg, ASR(reg, ImmInt(31)))
        instructions += BranchLinkNE(Label("p_throw_overflow_error"))
        addRuntimeError(Overflow)
        addUnusedReg(rReg)
      case Div(l, r, _) =>
        val rReg = getFreeReg()
        instructions ++= transExp(l, reg)
        instructions ++= transExp(r, rReg)
        // Needs to be in R0 and R1 for "__aeabi_idiv"
        instructions += Mov(R0, reg)
        instructions += Mov(R1, rReg)
        // Runtime error check
        instructions += BranchLink(Label("p_check_divide_by_zero"))
        addRuntimeError(DivideByZero)
        // Divide function
        instructions += BranchLink(Label("__aeabi_idiv"))
        addUnusedReg(rReg)
        instructions += Mov(reg, R0)
      case Mod(l, r, _) =>
        val rReg = getFreeReg()
        instructions ++= transExp(l, reg)
        instructions ++= transExp(r, rReg)
        // Runtime error check
        instructions += BranchLink(Label("p_check_divide_by_zero"))
        addRuntimeError(DivideByZero)
        // Needs to be in R0 and R1 for "__aeabi_idivmod"
        instructions += Mov(R0, reg)
        instructions += Mov(R1, rReg)
        // Runtime error check
        instructions += BranchLink(Label("p_check_divide_by_zero"))
        addRuntimeError(DivideByZero)
        // Mod function
        instructions += BranchLink(Label("__aeabi_idivmod"))
        addUnusedReg(rReg)
        instructions += Mov(reg, R1)
      case Plus(l, r, _) =>
        val rReg = getFreeReg()
        instructions ++= transExp(l, reg)
        instructions ++= transExp(r, rReg)
        instructions += AddS(reg, reg, rReg)
        // Runtime error check
        instructions += BranchLinkVS(Label("p_throw_overflow_error"))
        addRuntimeError(Overflow)
        addUnusedReg(rReg)
      case frontend.Rules.Sub(l, r, _) =>
        val rReg = getFreeReg()
        instructions ++= transExp(l, reg)
        instructions ++= transExp(r, rReg)
        instructions += InstructionSet.Sub(reg, reg, rReg)
        // Runtime error check
        instructions += BranchLinkVS(Label("p_throw_overflow_error"))
        addRuntimeError(Overflow)
        addUnusedReg(rReg)
      case frontend.Rules.And(l, r, _) =>
        val rReg = getFreeReg()
        instructions ++= transExp(l, reg)
        instructions ++= transExp(r, rReg)
        instructions += InstructionSet.And(reg, reg, rReg)
        addUnusedReg(rReg)
      case frontend.Rules.Or(l, r, _) =>
        val rReg = getFreeReg()
        instructions ++= transExp(l, reg)
        instructions ++= transExp(r, rReg)
        instructions += InstructionSet.Or(reg, reg, rReg)
        addUnusedReg(rReg)
      // Comparison binary operators
      case cmpOp => instructions ++= transCond(cmpOp, reg)
    }
    instructions
  }

  /* Translates an expression operator to the internal representation. */
  private def transExp(e: Expr, reg: Reg): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    // TODO: Determine free registers
    e match {
      case IntLiter(n, _)  => instructions += Ldr(reg, ImmMem(n))
      case BoolLiter(b, _) => instructions += Mov(reg, ImmInt(boolToInt(b)))
      // TODO: escaped character
      case CharLiter(c, _) => instructions += Mov(reg, ImmChar(c))
      case str: StrLiter   => instructions ++= transStrLiter(str, reg)
      case PairLiter(_)    => instructions += Ldr(reg, ImmMem(0))
      // TODO: track variable location
      case id: Ident =>
        val (index, t) = varTable.apply(id)
        val spOffset = currentSP - index
        t match {
          case CharT | BoolT =>
            instructions += LdrB(reg, RegisterOffset(SP, spOffset))
          case _ => instructions += Ldr(reg, RegisterOffset(SP, spOffset))
        }
      case ArrayElem(id, es, _) => instructions ++= loadArrayElem(id, es, reg)
      case e: UnOp              => instructions ++= transUnOp(e, reg)
      case e: BinOp             => instructions ++= transBinOp(e, reg)
    }
    instructions
  }

  private def getFreeReg(): Reg = {
    if (freeRegs.isEmpty) {
      // TODO: free up regs?
      return R0
    }
    val reg = freeRegs(0)
    freeRegs.remove(0)
    reg
  }

  private def addUnusedReg(r: Reg): Unit = {
    r +=: freeRegs
  }

  private def boolToInt(b: Boolean): Int = {
    if (b) { 1 }
    else { 0 }
  }

  private def getBaseTypeSize(t: Type): Int = {
    t match {
      case IntT           => INT_SIZE
      case BoolT          => BOOL_SIZE
      case CharT          => CHAR_SIZE
      case StringT        => STR_SIZE
      case ArrayT(innerT) => ARRAY_SIZE
      case Pair(_, _)     => PAIR_SIZE
      case _              => -1
    }
  }

  private def getPairElemTypeSize(pairType: PairElemType): Int = {
    pairType match {
      case PairElemPair        => PAIR_SIZE
      case PairElemT(baseType) => getBaseTypeSize(baseType)
    }
  }

}
