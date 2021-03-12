package backend

import backend.IR.InstructionSet._
import backend.IR.Operand._
import backend.IR.Condition._
import backend.Peephole._
import scala.collection._

object PeepholeBranch {

  def peepBranch(
      op1: Operand,
      op2: Operand,
      remainingHead: Instruction,
      remainingTail: mutable.ListBuffer[Instruction],
      optimised: mutable.ListBuffer[Instruction]
  ): Unit = {
    (op1, op2) match {
      // Comparison is true
      case (ImmInt(0), ImmInt(0)) | (ImmInt(1), ImmInt(1)) =>
        remainingTail.head match {
          case BranchCond(EQ, label) =>
            // Branch to necessary label
            optimised += Branch(label)
          case _ => optimise(remainingHead, remainingTail, optimised)
        }
      // Comparison is false
      case (ImmInt(0), ImmInt(1)) | (ImmInt(1), ImmInt(0)) =>
        remainingTail.head match {
          case BranchCond(EQ, label) =>
            // LABEL will never be required
            ignoreBlocks += label
            // Optimise rest of the instructions
            optimise(remainingTail.tail, optimised)
          case _ => optimise(remainingHead, remainingTail, optimised)
        }
      case _ => optimise(remainingHead, remainingTail, optimised)
    }
  }
}
