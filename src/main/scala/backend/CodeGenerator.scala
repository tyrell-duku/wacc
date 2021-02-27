package backend

import InstructionSet._
import frontend.Rules._
import scala.collection.mutable.ListBuffer

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

  private val INT_SIZE = 4
  private val CHAR_SIZE = 1
  private val BOOL_SIZE = 1
  private val STR_SIZE = 4
  private val ARRAY_SIZE = 4

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
    var toAdd =
      ListBuffer(
        Ldr(resultReg, ImmMem(0)),
        Pop(ListBuffer(PC)),
        Ltorg
      )
    instructions ++= toAdd
    (dataTable.table.toList, List((Label("main"), instructions.toList)))
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
      case Print(e)         => transPrint(e)
      case PrintLn(e)       =>
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
        val (index, t) = varTable.apply(id)
        t
      case ArrayElem(id, _, _) =>
        val (index, ArrayT(t)) = varTable.apply(id)
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

  private def transPrint(e: Expr): Unit = {
    val t = getExprType(e)
    t match {
      case CharT =>
        val freeReg = getFreeReg()
        transExp(e, freeReg)
        instructions += Mov(resultReg, freeReg)
        instructions += BranchLink(Label("putchar"))
      case IntT  =>
      case BoolT =>
    }
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

    assignRHS(t, aRHS, currentSP)
  }

  private def assignRHS(t: Type, aRHS: AssignRHS, spIndex: Int): Unit = {
    val spOffset = ImmInt(spIndex)
    instructions += InstructionSet.Sub(SP, SP, spOffset)
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
          instructions += StrB(freeReg, RegAdd(SP))
        } else {
          instructions += Str(freeReg, RegAdd(SP))
        }
        instructions += Add(SP, SP, spOffset)
      case ArrayT(t) =>
        aRHS match {
          case ArrayLiter(arr, _) =>
            instructions ++= ListBuffer[Instruction](
              InstructionSet.Sub(SP, SP, ImmInt(ARRAY_SIZE))
            )
            var rawList = List.empty[Expr]
            if (!arr.isEmpty) {
              rawList = arr.get
            }

            val baseTSize = getBaseTypeSize(t)
            var rawSize = 4 + (rawList.size * baseTSize)
            val arrayReg = getFreeReg()
            val tempReg = getFreeReg()
            val elemReg = getFreeReg()

            instructions ++= ListBuffer[Instruction](
              Ldr(arrayReg, ImmMem(rawSize)),
              BranchLink(Label("malloc")),
              Mov(tempReg, arrayReg)
            )

            if (rawSize == 4) {
              instructions ++= ListBuffer[Instruction](
                Ldr(elemReg, ImmMem(0))
              )
            } else {

              for (index <- 0 until rawList.size) {
                t match {
                  case IntT =>
                    val IntLiter(i, _) = rawList(index)
                    instructions ++= ListBuffer[Instruction](
                      Ldr(elemReg, ImmMem(i))
                    )
                  case BoolT =>
                    val BoolLiter(b, _) = rawList(index)
                    instructions ++= ListBuffer[Instruction](
                      Ldr(elemReg, ImmMem(boolToInt(b)))
                    )
                  case CharT =>
                    val CharLiter(c, _) = rawList(index)
                    instructions ++= ListBuffer[Instruction](
                      Ldr(elemReg, ImmChar(c))
                    )
                  case StringT =>
                    val StrLiter(str, pos) = rawList(index)
                    val label = dataTable.addDataEntry(StrLiter(str, pos))

                    instructions ++= ListBuffer[Instruction](
                      Ldr(elemReg, DataLabel(label))
                    )
                  case ArrayT(_) =>
                  case _         =>
                }

                instructions ++= ListBuffer[Instruction](
                  StrOffset(elemReg, tempReg, 4 + (index * baseTSize))
                )
              }
            }

            instructions ++= ListBuffer[Instruction](
              Ldr(elemReg, ImmMem(rawList.size)),
              Str(elemReg, RegAdd(tempReg)),
              Str(tempReg, RegAdd(SP)),
              Add(SP, SP, ImmInt(ARRAY_SIZE))
            )
          case _ => ListBuffer.empty[Instruction]
        }
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
        assignRHS(t, aRHS, index)
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

  private def transArrayElem(es: List[Expr]): Unit = {
    // TODO: symbol table from semanticChecker required
    val st = null
    val length = es.length
    val typeSize = getBaseTypeSize(es(0).getType(st))
    val lb = ListBuffer.empty[Instruction]

    lb += Ldr(R4, ImmMem(4 + typeSize * length))
    lb += BranchLink(Label("malloc"))
    lb += Mov(R4, R0)

    for (i <- 1 to length) {
      lb += Ldr(R5, ImmMem(0))
      lb += StrOffset(R5, R4, typeSize * i)
    }

    lb += Str(R5, RegAdd(R4))
    instructions ++= lb
  }

  /* Translates unary operator OP to the internal representation. */
  private def transUnOp(op: UnOp, reg: Reg): Unit = {
    op match {
      case Chr(e, _)   => transExp(e, reg)
      case Len(e, pos) => ListBuffer.empty
      case Negation(e, _) =>
        transExp(e, reg)
        instructions += NegInstr(reg, reg)
      case Not(e, _) =>
        transExp(e, reg)
        instructions += Eor(reg, reg, ImmInt(1))
      case Ord(e, _) =>
        transExp(e, reg)
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
        instructions += InstructionSet.Sub(SP, SP, ImmInt(index))
        t match {
          case CharT | BoolT => instructions += LdrB(reg, RegAdd(SP))
          case _             => instructions += Ldr(reg, RegAdd(SP))
        }
        instructions += InstructionSet.Add(SP, SP, ImmInt(index))
      case ArrayElem(id, es, _) => transArrayElem(es)
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
