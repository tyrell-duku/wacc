package backend

import backend.IR.InstructionSet._
import backend.IR.Operand._
import backend.IR.Condition._
import backend.Peephole.optimise
import scala.collection._

object PeepholeMov {

  /* Remove redundant Mov(r1,op1), Mov(rd, r1) -> Mov(rd, op1) */
  def peepholeMov(
      r1: Reg,
      op1: Operand,
      rd: Reg,
      r2: Operand,
      instructions: mutable.ListBuffer[Instruction],
      optimised: mutable.ListBuffer[Instruction]
  ): Unit = {
    // Check destReg != op1
    if (rd != op1) {
      optimise(Mov(rd, op1), instructions, optimised)
    } else {
      optimise(instructions.head, instructions.tail, optimised)
    }
  }
}
