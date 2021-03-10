package backend

import backend.IR.InstructionSet._
import backend.IR.Operand._
import backend.IR.Condition._
import backend.Peephole._

import scala.collection.mutable.ListBuffer
import scala.math._

object PeepholeStrong {

  def log2(i: Int): Double = {
    log(i) / log(2.0)
  }

  def peepholeStrong(
      r1: Reg,
      op1: LoadOperand,
      r2: Reg,
      op2: LoadOperand,
      instructionsBuff: ListBuffer[Instruction],
      remainingTail: ListBuffer[Instruction]
  ): ListBuffer[Instruction] = {
    val operation = remainingTail.head
    if (operation == SMul(r1, r2, r1, r2)) {
      val ImmMem(n) = op2
      var shiftAmount = log2(n)
      if (shiftAmount == floor(shiftAmount)) {
        instructionsBuff += Ldr(r1, op1)
        instructionsBuff += Mov(r1, LSL(r1, ImmInt(shiftAmount.toInt)))

        instructionsBuff ++= optimise(
          remainingTail.tail.tail.head,
          remainingTail.tail.tail.tail
        )
        return instructionsBuff
      }
    }
    instructionsBuff += Ldr(r1, op1)
    instructionsBuff ++= optimise(Ldr(r2, op2), remainingTail)
    instructionsBuff
  }
}
