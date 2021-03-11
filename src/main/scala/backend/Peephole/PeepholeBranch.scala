package backend

import backend.IR.InstructionSet._
import backend.IR.Operand._
import backend.IR.Condition._
import backend.Peephole._
import scala.collection._

object PeepholeBranch {

  def peepholeBranch(
      op1: Operand,
      op2: Operand,
      remainingHead: Instruction,
      remainingTail: mutable.ListBuffer[Instruction],
      optimised: mutable.ListBuffer[Instruction]
  ): Unit = {
    op1 match {
      case ImmInt(0) =>
        if (op2 == ImmInt(0)) {
          remainingTail.head match {
            case BranchCond(EQ, label) =>
              optimised += Branch(label)
              return
            case _ =>
          }
        } else if (op2 == ImmInt(1)) {
          remainingTail.head match {
            case BranchCond(EQ, label) =>
              ignoreBlocks += label
              optimise(remainingTail.tail, optimised)
              return
            case _ =>
          }
        }
      case ImmInt(1) =>
        if (op2 == ImmInt(1)) {
          remainingTail.head match {
            case BranchCond(EQ, label) =>
              optimised += Branch(label)
              return
            case _ =>
          }
        } else if (op2 == ImmInt(0)) {
          remainingTail.head match {
            case BranchCond(EQ, label) =>
              ignoreBlocks += label
              optimise(remainingTail.tail, optimised)
              return
            case _ =>
          }
        }
      case _ =>
    }
    optimise(remainingHead, remainingTail, optimised)
  }
}
