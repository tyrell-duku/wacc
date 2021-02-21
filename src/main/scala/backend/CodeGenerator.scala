package backend

import InstructionSet._
import scala.collection.mutable.ListBuffer
import Rules._
import backend.LoadOperand

object CodeGenerator {
  private var instructions: ListBuffer[Instruction] =
    ListBuffer.empty[Instruction]

  final val allRegs: ListBuffer[Reg] =
    ListBuffer(R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12)

  final val resultReg: Reg = R0

  var varTable = Map.empty[Ident, Reg]

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

  def transProg(prog: Program): ListBuffer[Instruction] = {
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
    instructions
  }

  private def transStat(
      stat: Stat,
      regs: ListBuffer[Reg]
  ): ListBuffer[Instruction] = {
    stat match {
      case EqIdent(t, i, r) =>
      case EqAssign(l, r)   =>
      case Read(lhs)        =>
      case Free(e)          =>
      case Return(e)        =>
      case Exit(e)          => return transExit(e, regs)
      case Print(e)         =>
      case PrintLn(e)       =>
      case If(cond, s1, s2) =>
      case While(cond, s)   =>
      case Begin(s)         =>
      case Seq(statList)    =>
      case _                => // ignore Skip
    }
    ListBuffer.empty[Instruction]
  }

  private def transExit(
      e: Expr,
      regs: ListBuffer[Reg]
  ): ListBuffer[Instruction] = {
    e match {
      case IntLiter(n, _) =>
        val freeReg = regs.filter(r => r != R0)(0)
        ListBuffer[Instruction](
          Ldr(freeReg, ImmMem(n)),
          Mov(R0, freeReg),
          BranchLink("exit")
        )
      case _ => transExp(e)
    }
  }

  private def transExp(
      e: Expr
  ): ListBuffer[Instruction] = {
    ListBuffer.empty[Instruction]
  }

}
