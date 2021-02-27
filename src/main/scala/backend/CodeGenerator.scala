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

  var varTable = Map.empty[Ident, Int]

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
        Pop(ListBuffer(PC))
      )
    instructions ++= toAdd
    (dataTable.table.toList, List((Label("main"), instructions.toList)))
  }

  private def transStat(
      stat: Stat
  ) = {
    stat match {
      case EqIdent(t, i, r) => transEqIdent(t, i, r)
      case EqAssign(l, r)   =>
      case Read(lhs)        =>
      case Free(e)          =>
      case Return(e)        =>
      case Exit(e)          => transExit(e)
      case Print(e)         =>
      case PrintLn(e)       =>
      case If(cond, s1, s2) =>
      case While(cond, s)   =>
      case Begin(s)         =>
      case Seq(statList)    =>
      case _                =>
    }
  }

  private def transExit(
      e: Expr
  ): Unit = {
    val freeReg = getFreeReg()
    transExp(e)
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
    var freeReg = getFreeReg()

    val spOffset = ImmInt(getBaseTypeSize(t))
    instructions += InstructionSet.Sub(SP, SP, spOffset)

    t match {
      case IntT | CharT | BoolT | StringT =>
        aRHS match {
          case ex: Expr          => transExp(ex)
          case p: PairElem       =>
          case Call(id, args, _) =>
          case _                 => ListBuffer.empty[Instruction]
        }
        instructions ++= ListBuffer(
          Str(freeReg, RegAdd(SP)),
          Add(SP, SP, spOffset)
        )
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
                    val StrLiter(s, _) = rawList(index)
                    val label = dataTable.addDataEntry(s)

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

  private def transStrLiter(str: List[Character]): ListBuffer[Instruction] = {
    val curLabel = dataTable.addDataEntry(str)
    ListBuffer(Ldr(R1, DataLabel(curLabel)))
  }

  private def transArrayElem(es: List[Expr]): ListBuffer[Instruction] = {
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
    lb
  }

  /* Translates unary operator to the internal representation. */
  private def transUnOp(op: UnOp): ListBuffer[Instruction] = {
    op match {
      case Chr(e, pos)      => ListBuffer.empty
      case Len(e, pos)      => ListBuffer.empty
      case Negation(e, pos) => ListBuffer.empty
      case Not(e, pos) =>
        transExp(e) ++ ListBuffer(Eor(R4, R4, ImmInt(1)))
      case Ord(e, pos) => ListBuffer.empty
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
  private def transCond(op: BinOp): ListBuffer[Instruction] = {
    val lb = transExp(op.lExpr) ++ transExp(op.rExpr)
    val cmp = rulesCmpToInstrCmp(op)
    lb += Cmp(R4, R5)
    lb += MovCond(cmp, R4, ImmInt(1))
    lb += MovCond(cmp.oppositeCmp, R4, ImmInt(0))
    lb += Mov(R0, R4)
    lb
  }

  /* Translates a binary operator to the internal representation. */
  private def transBinOp(op: BinOp): ListBuffer[Instruction] = {
    // TODO: determine result register for l & r
    op match {
      case frontend.Rules.Mul(l, r, _) =>
        transExp(l) ++ transExp(r) += InstructionSet.Mul(R0, R1, R2)
      case Div(lExpr, rExpr, _) => ListBuffer.empty
      case Mod(lExpr, rExpr, _) => ListBuffer.empty
      case Plus(l, r, _) =>
        transExp(l) ++ transExp(r) += Add(R0, R1, R2)
      case frontend.Rules.Sub(l, r, _) =>
        transExp(l) ++ transExp(r) += InstructionSet.Sub(R0, R1, R2)
      case frontend.Rules.And(l, r, _) =>
        transExp(l) ++ transExp(r) += backend.InstructionSet.And(R5, R5, R6)
      case frontend.Rules.Or(l, r, _) =>
        transExp(l) ++ transExp(r) += backend.InstructionSet.Or(R5, R5, R6)
      // Comparison binary operators
      case cmpOp => transCond(cmpOp)
    }
  }

  /* Translates an expression operator to the internal representation. */
  private def transExp(e: Expr): ListBuffer[Instruction] = {
    // TODO: Determine free registers
    e match {
      case IntLiter(n, _) => ListBuffer(Mov(R1, ImmInt(n)))
      case BoolLiter(b, _) =>
        return ListBuffer
          .empty[Instruction] // return ListBuffer(Mov(R1, ImmInt(n)))
      // TODO: escaped character
      case CharLiter(c, _)  => ListBuffer(Mov(R1, ImmChar(c)))
      case StrLiter(str, _) => transStrLiter(str)
      case PairLiter(_)     => ListBuffer(Ldr(R1, ImmMem(0)))
      // TODO: track variable location
      case Ident(s, _)          => ListBuffer.empty[Instruction]
      case ArrayElem(id, es, _) => transArrayElem(es)
      case e: UnOp              => ListBuffer.empty[Instruction]
      case e: BinOp             => ListBuffer.empty[Instruction]
    }
  }

  private def getFreeReg(): Reg = {
    if (freeRegs.isEmpty) {
      //todo: free up regs?
      return R0
    }
    val reg = freeRegs(0)
    freeRegs.remove(0)
    reg
  }

  private def addUnusedReg(r: Reg): Unit = {
    freeRegs += r
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
      case _: PairType    => -1
      case _              => -1
    }
  }

}
