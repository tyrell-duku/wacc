package backend

import backend.IR.InstructionSet._
import backend.IR.Operand._
import backend.IR.Condition._
import backend.Peephole._

import scala.collection.mutable.ListBuffer

object PeepholeBranch {

  def peepholeBranch(
      op1: Operand,
      op2: Operand,
      instructionsBuff: ListBuffer[Instruction],
      remainingHead: Instruction,
      remainingTail: ListBuffer[Instruction]
  ): ListBuffer[Instruction] = {
    op1 match {
      case ImmInt(0) =>
        if (op2 == ImmInt(0)) {
          remainingTail.head match {
            case BranchCond(EQ, label) =>
              instructionsBuff += Branch(label)
              return instructionsBuff
            case _ =>
          }
        } else if (op2 == ImmInt(1)) {
          remainingTail.head match {
            case BranchCond(EQ, label) =>
              ignoreBlocks += label
              instructionsBuff ++= optimise(
                remainingTail.tail.head,
                remainingTail.tail.tail
              )
              return instructionsBuff
            case _ =>
          }
        }
      case ImmInt(1) =>
        if (op2 == ImmInt(1)) {
          remainingTail.head match {
            case BranchCond(EQ, label) =>
              instructionsBuff += Branch(label)
              return instructionsBuff
            case _ =>
          }
        } else if (op2 == ImmInt(0)) {
          remainingTail.head match {
            case BranchCond(EQ, label) =>
              ignoreBlocks += label
              instructionsBuff ++= optimise(
                remainingTail.tail.head,
                remainingTail.tail.tail
              )
              return instructionsBuff
            case _ =>
          }

        }
      case _ =>
    }
    instructionsBuff ++= optimise(remainingHead, remainingTail)
  }

}
