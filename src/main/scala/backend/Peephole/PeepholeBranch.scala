package backend

import backend.IR.InstructionSet._
import backend.IR.Operand._
import backend.IR.Condition._
import backend.Peephole._
import scala.collection._

object PeepholeBranch {

  def peepBranch(
      r1: Reg,
      op1: Operand,
      rd: Reg,
      op2: Operand,
      remainingTail: mutable.ListBuffer[Instruction],
      optimised: mutable.ListBuffer[Instruction]
  ): Unit = {
    if (r1 == rd) {
      (op1, op2) match {
        // Comparison is true
        case (ImmInt(0), ImmInt(0)) | (ImmInt(1), ImmInt(1)) =>
          remainingTail.head match {
            case BranchCond(EQ, label) =>
              // Branch to necessary label
              optimised += Branch(label)
            case _ =>
              continueOptimise(
                Mov(r1, op1),
                Cmp(rd, op2),
                remainingTail,
                optimised
              )
          }
        // Comparison is false
        case (ImmInt(0), ImmInt(1)) | (ImmInt(1), ImmInt(0)) =>
          remainingTail.head match {
            case BranchCond(EQ, label) =>
              // LABEL will never be required
              ignoreBlocks += label
              // Optimise rest of the instructions
              optimise(remainingTail.tail, optimised)
            case _ =>
              continueOptimise(
                Mov(r1, op1),
                Cmp(rd, op2),
                remainingTail,
                optimised
              )
          }
        case _ =>
          continueOptimise(Mov(r1, op1), Cmp(rd, op2), remainingTail, optimised)
      }
    } else {
      continueOptimise(Mov(r1, op1), Cmp(rd, op2), remainingTail, optimised)
    }
  }
}
