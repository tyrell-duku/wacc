package backend

import backend.IR.InstructionSet._
import backend.IR.Operand._
import backend.IR.Condition._
import backend.Peephole._
import CodeGenerator.{TRUE_INT, FALSE_INT}
import scala.collection._

object PeepholeBranch {

  /* Removes redundant branches */
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
        case (ImmInt(FALSE_INT), ImmInt(FALSE_INT)) |
            (ImmInt(TRUE_INT), ImmInt(TRUE_INT)) =>
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
        case (ImmInt(FALSE_INT), ImmInt(TRUE_INT)) |
            (ImmInt(TRUE_INT), ImmInt(FALSE_INT)) =>
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
