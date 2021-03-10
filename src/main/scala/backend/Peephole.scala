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
      instructionsBuff = compareMovs(instructions(0), remaining.tail)
    }
    (label, instructionsBuff.toList)
    // (label, instructions)
  }

  def compareMovs(
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
                instructionsBuff += Mov(rd, op1)
                instructionsBuff ++= compareMovs(
                  remainingTail.head,
                  remainingTail.tail
                )
                return instructionsBuff
              }
            case Cmp(rd, op2) =>
              if (rd == rd) {
                op1 match {
                  case ImmInt(0) =>
                    if (op2 == ImmInt(0)) {
                      remainingTail.head match {
                        case BranchCond(EQ, label) =>
                          println("optimiseing")
                          instructionsBuff += Branch(label)
                          println(instructionsBuff)
                          return instructionsBuff
                        case _ =>
                      }
                    } else if (op2 == ImmInt(1)) {
                      instructionsBuff ++= compareMovs(
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
                      instructionsBuff ++= compareMovs(
                        remainingTail.tail.head,
                        remainingTail.tail.tail
                      )
                      return instructionsBuff
                    }
                  case _ =>
                }
              }
            case _ =>
          }
        case _ =>
      }
      instructionsBuff += cur
      instructionsBuff ++= compareMovs(remainingHead, remainingTail)
      return instructionsBuff
    }
    instructionsBuff
  }

  def optimise(
      blocks: List[(Label, List[Instruction])]
  ): List[(Label, List[Instruction])] = {
    val returnBlocks = ListBuffer.empty[(Label, List[Instruction])]
    for (b <- blocks) {
      returnBlocks += optimiseBlock(b)
    }
    returnBlocks.toList
  }
}
