package backend

import backend.IR.InstructionSet._
import backend.IR.Operand._
import backend.IR.Condition._
import backend.Peephole.optimise

import scala.collection.mutable.ListBuffer

object PeepholeMov {

  def peepholeMov(
      op1: Operand,
      rd: Reg,
      instructionsBuff: ListBuffer[Instruction],
      remainingTail: ListBuffer[Instruction]
  ): ListBuffer[Instruction] = {
    instructionsBuff += Mov(rd, op1)
    instructionsBuff ++= optimise(
      remainingTail.head,
      remainingTail.tail
    )
    instructionsBuff
  }

}
