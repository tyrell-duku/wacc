package backend

import backend.IR.InstructionSet._
import backend.IR.Operand._
import backend.IR.Condition._
import backend.Peephole.optimise
import scala.collection._

object PeepholeMov {

  def peepholeMov(
      op1: Operand,
      rd: Reg,
      instructionsBuff: mutable.ListBuffer[Instruction],
      remainingTail: mutable.ListBuffer[Instruction]
  ): mutable.ListBuffer[Instruction] = {
    if (rd != op1) {
      instructionsBuff += Mov(rd, op1)
    }
    instructionsBuff ++= optimise(
      remainingTail.head,
      remainingTail.tail
    )
    instructionsBuff
  }

}
