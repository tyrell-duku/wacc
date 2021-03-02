package backend

import InstructionSet._
import frontend.Rules._
import scala.collection.mutable.ListBuffer
import PrintInstrs._

object CodeGenerator {
  private var instructions: ListBuffer[Instruction] =
    ListBuffer.empty[Instruction]

  final val allRegs: ListBuffer[Reg] =
    ListBuffer(R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12)

  var freeRegs = allRegs
  final val resultReg: Reg = R0
  freeRegs -= resultReg

  var varTable = Map.empty[Ident, (Int, Type)]
  var currentSP = 0

  private val dataTable = new DataTable
  private val funcTable = new FuncTable

  private val INT_SIZE = 4
  private val CHAR_SIZE = 1
  private val BOOL_SIZE = 1
  private val STR_SIZE = 4
  private val ARRAY_SIZE = 4
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

  def transProg(
      prog: Program
  ): (List[Data], List[(Label, List[Instruction])]) = {
    val Program(funcs, stat) = prog
    // val funcCode = funcs.map(transFunc)
    // instructions += funcCode
    instructions = ListBuffer(Push(ListBuffer(LR)))
    transStat(stat)
    addSP()
    var toAdd =
      ListBuffer(
        Ldr(resultReg, ImmMem(0)),
        Pop(ListBuffer(PC)),
        Ltorg
      )
    instructions ++= toAdd
    val funcList =
      ListBuffer((Label("main"), instructions.toList)) ++ funcTable.table
    (dataTable.table.toList, funcList.toList)
  }

  private def transStat(
      stat: Stat
  ): Unit = {
    stat match {
      case EqIdent(t, i, r) => transEqIdent(t, i, r)
      case EqAssign(l, r)   => transEqAssign(l, r)
      case Read(lhs)        =>
      case Free(e)          =>
      case Return(e)        =>
      case Exit(e)          => transExit(e)
      case Print(e)         => transPrint(e, false)
      case PrintLn(e)       => transPrint(e, true)
      case If(cond, s1, s2) =>
      case While(cond, s)   =>
      case Begin(s)         =>
      case Seq(statList)    => statList.map(transStat)
      case _                =>
    }
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
          t = t match { case ArrayT(inner) => inner }
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

  private def transPrint(e: Expr, isNewLine: Boolean): Unit = {
    val t = getExprType(e)
    val freeReg = getFreeReg()
    transExp(e, freeReg)
    instructions += Mov(resultReg, freeReg)
    t match {
      case CharT =>
        instructions += BranchLink(Label("putchar"))
      case IntT =>
        dataTable.addDataEntryWithLabel("msg_int", "%d\\0")
        instructions += BranchLink(Label("p_print_int"))
        funcTable.addEntry(intPrintInstrs)
      case BoolT =>
        dataTable.addDataEntryWithLabel("msg_true", "true\\0")
        dataTable.addDataEntryWithLabel("msg_false", "false\\0")
        instructions += BranchLink(Label("p_print_bool"))
        funcTable.addEntry(boolPrintInstrs)
      case StringT =>
        dataTable.addDataEntryWithLabel("msg_string", "%.*s\\0")
        instructions += BranchLink(Label("p_print_string"))
        funcTable.addEntry(stringPrintInstrs)
      case Pair(null, null) =>
        dataTable.addDataEntryWithLabel("msg_reference", "%p\\0")
        instructions += BranchLink(Label("p_print_reference"))
        funcTable.addEntry(referencePrintInstrs)
      case ArrayT(CharT) =>
        dataTable.addDataEntryWithLabel("msg_string", "%.*s\\0")
        instructions += BranchLink(Label("p_print_string"))
        funcTable.addEntry(stringPrintInstrs)
      case _ =>
    }
    if (isNewLine) {
      instructions += BranchLink(Label("p_print_ln"))
      dataTable.addDataEntryWithLabel("msg_new_line", "\\0")
      funcTable.addEntry(newLinePrintInstrs)
    }
    addUnusedReg(freeReg)
  }

  private def transExit(
      e: Expr
  ): Unit = {
    val freeReg = getFreeReg()
    transExp(e, freeReg)
    instructions ++= ListBuffer[Instruction](
      Mov(R0, freeReg),
      BranchLink(Label("exit"))
    )
    addUnusedReg(freeReg)
  }

  private def transEqIdent(
      t: Type,
      id: Ident,
      aRHS: AssignRHS
  ): Unit = {

    currentSP += getBaseTypeSize(t)
    varTable += (id -> (currentSP, t))
    instructions += InstructionSet.Sub(SP, SP, ImmInt(getBaseTypeSize(t)))

    assignRHS(t, aRHS, 0)
  }

  private def subtractSP(): Unit = {
    var curSp = currentSP
    while (curSp > MAX_INT_IMM) {
      curSp -= MAX_INT_IMM
      instructions += InstructionSet.Sub(SP, SP, ImmInt(MAX_INT_IMM))
    }
    instructions += InstructionSet.Sub(SP, SP, ImmInt(curSp))
  }

  private def addSP(): Unit = {
    var curSp = currentSP
    while (curSp > MAX_INT_IMM) {
      curSp -= MAX_INT_IMM
      instructions += InstructionSet.Add(SP, SP, ImmInt(MAX_INT_IMM))
    }
    instructions += InstructionSet.Add(SP, SP, ImmInt(curSp))
  }

  private def assignRHS(t: Type, aRHS: AssignRHS, spOffset: Int): Unit = {
    val freeReg = getFreeReg()
    t match {
      case IntT | CharT | BoolT | StringT =>
        aRHS match {
          case ex: Expr          => transExp(ex, freeReg)
          case p: PairElem       =>
          case Call(id, args, _) =>
          case _                 => ListBuffer.empty[Instruction]
        }
        if (t == CharT || t == BoolT) {
          instructions += StrB(freeReg, RegisterOffset(SP, spOffset))
        } else {
          instructions += Str(freeReg, RegisterOffset(SP, spOffset))
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
            transExp(arr(i), nextFreeReg)
            instructions += StrB(nextFreeReg, RegisterOffset(freeReg, i + 4))
          }
        } else {
          for (i <- 0 until listSize) {
            transExp(arr(i), nextFreeReg)
            instructions += Str(
              nextFreeReg,
              RegisterOffset(freeReg, (i + 1) * 4)
            )
          }
        }
        instructions += Ldr(nextFreeReg, ImmMem(listSize))
        instructions += Str(nextFreeReg, RegAdd(freeReg))
        instructions += Str(freeReg, RegAdd(SP))
        addUnusedReg(nextFreeReg)

      case _: PairType => ListBuffer.empty[Instruction]
      case _           => ListBuffer.empty[Instruction]
    }
    addUnusedReg(freeReg)
  }

  private def transEqAssign(
      aLHS: AssignLHS,
      aRHS: AssignRHS
  ): Unit = {
    aLHS match {
      case elem: PairElem =>
      case id: Ident =>
        val (index, t) = varTable.apply(id)
        assignRHS(t, aRHS, currentSP - index)
      case arrayElem: ArrayElem =>
    }
  }

  private def transStrLiter(
      str: StrLiter,
      reg: Reg
  ): Unit = {
    val curLabel = dataTable.addDataEntry(str)
    instructions += Ldr(reg, DataLabel(curLabel))
  }

  private def transArrayElem(id: Ident, es: List[Expr], reg: Reg): Unit = {
    val (index, t) = varTable.apply(id)
    val baseTypeSize = getBaseTypeSize(t)
    val spOffset = currentSP - index
    instructions += InstructionSet.Add(reg, SP, ImmInt(spOffset))
    val nextReg = getFreeReg()
    for (exp <- es) {
      transExp(exp, nextReg)
      instructions += Ldr(reg, RegAdd(reg))

      //TODO: Out of bounds check

      instructions += Add(reg, reg, ImmInt(INT_SIZE))
      if (t == CharT || t == BoolT) {
        instructions += Add(reg, reg, nextReg)
      } else {
        instructions += Add(reg, reg, LSL(nextReg, ImmInt(2)))
      }
    }
    addUnusedReg(nextReg)
    if (t == CharT || t == BoolT) {
      instructions += LdrB(reg, RegAdd(reg))
    } else {
      instructions += Ldr(reg, RegAdd(reg))
    }
  }

  /* Translates unary operator OP to the internal representation. */
  private def transUnOp(op: UnOp, reg: Reg): Unit = {
    op match {
      case Chr(e, _) => transExp(e, reg)
      case Len(id: Ident, _) =>
        val (index, _) = varTable.apply(id)
        instructions += InstructionSet.Sub(SP, SP, ImmInt(index))
        instructions += Ldr(reg, RegAdd(SP))
        instructions += Ldr(reg, RegAdd(reg))
        instructions += InstructionSet.Add(SP, SP, ImmInt(index))
      case Negation(e, _) =>
        transExp(e, reg)
        instructions += NegInstr(reg, reg)
      case Not(e, _) =>
        transExp(e, reg)
        instructions += Eor(reg, reg, ImmInt(1))
      case Ord(e, _) =>
        transExp(e, reg)
      case _ => // throw error?
    }
  }

  /* Changes a comparison operator to the Condition equivalent.
     Returns null when not a comparison operator. */
  private def rulesCmpToInstrCmp(cmp: BinOp): Condition = {
    cmp match {
      case GT(lExpr, rExpr, pos)       => backend.GT
      case GTE(lExpr, rExpr, pos)      => backend.GTE
      case LT(lExpr, rExpr, pos)       => backend.LT
      case LTE(lExpr, rExpr, pos)      => backend.LTE
      case Equal(lExpr, rExpr, pos)    => backend.EQ
      case NotEqual(lExpr, rExpr, pos) => backend.NE
      case _                           => null // Undefined
    }
  }

  /* Translates a comparator operator to the internal representation. */
  private def transCond(op: BinOp, reg: Reg): Unit = {
    val rReg = getFreeReg()
    transExp(op.lExpr, reg)
    transExp(op.rExpr, rReg)
    val cmp = rulesCmpToInstrCmp(op)
    instructions += Cmp(reg, rReg)
    addUnusedReg(rReg)
    instructions += MovCond(cmp, reg, ImmInt(1))
    instructions += MovCond(cmp.oppositeCmp, reg, ImmInt(0))
    // lb += Mov(R0, R4)
  }

  /* Translates a binary operator to the internal representation. */
  private def transBinOp(op: BinOp, reg: Reg): Unit = {
    // TODO: determine result register for l & r
    op match {
      case frontend.Rules.Mul(l, r, _) =>
        val rReg = getFreeReg()
        transExp(l, reg)
        transExp(r, rReg)
        instructions += InstructionSet.Mul(reg, reg, rReg)
        addUnusedReg(rReg)
      case Div(l, r, _) =>
        val rReg = getFreeReg()
        // TODO: make sure R0 and R1 are free
        transExp(l, reg)
        transExp(r, rReg)
        // Needs to be in R0 and R1 for "__aeabi_idiv"
        instructions += Mov(reg, R0)
        instructions += Mov(rReg, R1)
        // Divide function
        instructions += BranchLink(Label("__aeabi_idiv"))
        addUnusedReg(rReg)
        instructions += Mov(reg, R0)
      case Mod(l, r, _) =>
        val rReg = getFreeReg()
        // TODO: make sure R0 and R1 are free
        transExp(l, reg)
        transExp(r, rReg)
        // Needs to be in R0 and R1 for "__aeabi_idivmod"
        instructions += Mov(reg, R0)
        instructions += Mov(rReg, R1)
        // Mod function
        instructions += BranchLink(Label("__aeabi_idivmod"))
        addUnusedReg(rReg)
        instructions += Mov(reg, R1)
      case Plus(l, r, _) =>
        val rReg = getFreeReg()
        transExp(l, reg)
        transExp(r, rReg)
        instructions += Add(reg, reg, rReg)
        addUnusedReg(rReg)
      case frontend.Rules.Sub(l, r, _) =>
        val rReg = getFreeReg()
        transExp(l, reg)
        transExp(r, rReg)
        instructions += InstructionSet.Sub(reg, reg, rReg)
        addUnusedReg(rReg)
      case frontend.Rules.And(l, r, _) =>
        val rReg = getFreeReg()
        transExp(l, reg)
        transExp(r, rReg)
        instructions += InstructionSet.And(reg, reg, rReg)
        addUnusedReg(rReg)
      case frontend.Rules.Or(l, r, _) =>
        val rReg = getFreeReg()
        transExp(l, reg)
        transExp(r, rReg)
        instructions += InstructionSet.Or(reg, reg, rReg)
        addUnusedReg(rReg)
      // Comparison binary operators
      case cmpOp => transCond(cmpOp, reg)
    }
  }

  /* Translates an expression operator to the internal representation. */
  private def transExp(e: Expr, reg: Reg): Unit = {
    // TODO: Determine free registers
    e match {
      case IntLiter(n, _)  => instructions += Ldr(reg, ImmMem(n))
      case BoolLiter(b, _) => instructions += Mov(reg, ImmInt(boolToInt(b)))
      // TODO: escaped character
      case CharLiter(c, _) => instructions += Mov(reg, ImmChar(c))
      case str: StrLiter   => transStrLiter(str, reg)
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
      case ArrayElem(id, es, _) => transArrayElem(id, es, reg)
      case e: UnOp              => transUnOp(e, reg)
      case e: BinOp             => transBinOp(e, reg)
    }
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
      // CODEME
      case _: PairType => -1
      case _           => -1
    }
  }

}
