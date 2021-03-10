package backend

import java.io.{File, FileWriter}
import backend.IR.InstructionSet._
import backend.IR.Operand._
import backend.IR.Condition._

import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

object Peephole {

  def optimiseBlock(
      block: (Label, List[Instruction])
  ): (Label, List[Instruction]) = {
    val (label, instructions) = block
    val remaining = ListBuffer.empty[Instruction]
    remaining.addAll(instructions)
    var instructionsBuff = ListBuffer.empty[Instruction]

    if (!instructions.isEmpty) {
      instructionsBuff = optimise(instructions(0), remaining.tail)
    }
    (label, instructionsBuff.toList)
  }

  def redundantMov(
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

  def redundantBranching(
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
          instructionsBuff ++= optimise(
            remainingTail.tail.head,
            remainingTail.tail.tail
          )
          return instructionsBuff
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
          instructionsBuff ++= optimise(
            remainingTail.tail.head,
            remainingTail.tail.tail
          )
          return instructionsBuff
        }
      case _ =>
    }
    instructionsBuff ++= optimise(remainingHead, remainingTail)
  }

  def optimise(
      cur: Instruction,
      remaining: ListBuffer[Instruction]
  ): ListBuffer[Instruction] = {
    val instructionsBuff = ListBuffer.empty[Instruction]
    if (remaining.isEmpty) {
      instructionsBuff += cur
    } else {
      val remainingHead = remaining.head
      val remainingTail = remaining.tail

      cur match {
        case Mov(r1, op1) =>
          remainingHead match {
            case Mov(rd, r2) =>
              if (r1 == r2) {
                return redundantMov(op1, rd, instructionsBuff, remainingTail)
              }
            case Cmp(rd, op2) =>
              if (r1 == rd) {
                return redundantBranching(
                  op1,
                  op2,
                  instructionsBuff,
                  remainingHead,
                  remainingTail
                )
              }
            case _ =>
          }
        case _ =>
      }
      instructionsBuff += cur
      instructionsBuff ++= optimise(remainingHead, remainingTail)
      return instructionsBuff
    }
    instructionsBuff
  }

  def optimiseBlocks(
      blocks: List[(Label, List[Instruction])]
  ): List[(Label, List[Instruction])] = {
    val returnBlocks = ListBuffer.empty[(Label, List[Instruction])]
    for (b <- blocks) {
      returnBlocks += optimiseBlock(b)
    }
    returnBlocks.toList
  }
}
