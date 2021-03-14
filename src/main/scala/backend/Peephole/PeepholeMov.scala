package backend

import backend.IR.InstructionSet._
import backend.IR.Operand._
import backend.IR.Condition._
import backend.Peephole._
import scala.collection._

object PeepholeMov {

  /* Remove redundant Mov(r1,op1), Mov(rd, r1) -> Mov(rd, op1) */
  def peepMov(
      r1: Reg,
      op1: Operand,
      rd: Reg,
      r2: Operand,
      instructions: mutable.ListBuffer[Instruction],
      optimised: mutable.ListBuffer[Instruction]
  ): Unit = {
    if (r1 == r2) {
      // Check destReg != op1
      if (rd != op1) {
        optimise(Mov(rd, op1), instructions, optimised)
      } else {
        // If rd == op1, no need to add intructions
        optimise(instructions.head, instructions.tail, optimised)
      }
    } else {
      continueOptimise(Mov(r1, op1), Mov(rd, r2), instructions, optimised)
    }
  }

  /* Removes redundant Load instructions */
  def peepStrLdr(
      r1: Reg,
      op1: Address,
      r2: Reg,
      op2: LoadOperand,
      instructions: mutable.ListBuffer[Instruction],
      optimised: mutable.ListBuffer[Instruction],
      isByte: Boolean
  ): Unit = {
    if (r1 == r2 && op1 == op2) {
      // Skip over the Load intruction
      if (isByte) {
        optimise(StrB(r1, op1), instructions, optimised)
      } else {
        optimise(Str(r1, op1), instructions, optimised)
      }
    } else {
      if (isByte) {
        continueOptimise(StrB(r1, op1), LdrSB(r2, op2), instructions, optimised)
      } else {
        continueOptimise(Str(r1, op1), Ldr(r2, op2), instructions, optimised)
      }
    }
  }
}
