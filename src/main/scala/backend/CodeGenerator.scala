package backend

import InstructionSet._
import frontend.Rules._

import scala.collection.mutable.ListBuffer

object CodeGenerator {
  private var instructions: ListBuffer[Instruction] =
    ListBuffer.empty[Instruction]

  final val allRegs: ListBuffer[Reg] =
    ListBuffer(R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12)

  final val resultReg: Reg = R0
  val dataTable = new DataTable()

  var varTable = Map.empty[Ident, Reg]

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
    var toAdd =
      transStat(stat, allRegs) ++ ListBuffer(
        Ldr(resultReg, ImmMem(0)),
        Pop(ListBuffer(PC))
      )
    instructions ++= toAdd
    (dataTable.table.toList, List((Label("main"), instructions.toList)))
  }

  private def transStat(
      stat: Stat,
      regs: ListBuffer[Reg]
  ): ListBuffer[Instruction] = {
    stat match {
      case EqIdent(t, i, r) => transEqIdent(t, i, r, regs)
      case EqAssign(l, r)   => ListBuffer.empty[Instruction]
      case Read(lhs)        => ListBuffer.empty[Instruction]
      case Free(e)          => ListBuffer.empty[Instruction]
      case Return(e)        => ListBuffer.empty[Instruction]
      case Exit(e)          => transExit(e, regs)
      case Print(e)         => ListBuffer.empty[Instruction]
      case PrintLn(e)       => ListBuffer.empty[Instruction]
      case If(cond, s1, s2) => ListBuffer.empty[Instruction]
      case While(cond, s)   => ListBuffer.empty[Instruction]
      case Begin(s)         => ListBuffer.empty[Instruction]
      case Seq(statList)    => ListBuffer.empty[Instruction]
      case _                => ListBuffer.empty[Instruction]
    }
  }

  private def transExit(
      e: Expr,
      regs: ListBuffer[Reg]
  ): ListBuffer[Instruction] = {
    e match {
      case IntLiter(n, _) =>
        val freeReg = getFreeReg(regs, ListBuffer(R0))
        ListBuffer[Instruction](
          Ldr(freeReg, ImmMem(n)),
          Mov(R0, freeReg),
          BranchLink(Label("exit"))
        )
      case _ => transExp(e)
    }
  }

  private def transEqIdent(
      t: Type,
      id: Ident,
      aRHS: AssignRHS,
      regs: ListBuffer[Reg]
  ): ListBuffer[Instruction] = {
    ListBuffer.empty[Instruction]
  }

  private def transExp(
      e: Expr
  ): ListBuffer[Instruction] = {
    ListBuffer.empty[Instruction]
  }

  private def getFreeReg(
      regs: ListBuffer[Reg],
      regsInUse: ListBuffer[Reg]
  ): Reg = {
    regs.filter(r => !regsInUse.contains(r))(0)
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
