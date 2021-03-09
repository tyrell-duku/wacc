package backend

import java.io.{File, FileWriter}
import backend.IR.InstructionSet._
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
        case Mov(r1, op) =>
          remainingHead match {
            case Mov(rd, r2) =>
              if (r1 == r2) {
                instructionsBuff += Mov(rd, op)
                instructionsBuff ++= compareMovs(
                  remainingTail.head,
                  remainingTail.tail
                )
              } else {
                instructionsBuff += cur
                instructionsBuff ++= compareMovs(remainingHead, remainingTail)
              }
            case _ =>
              instructionsBuff += cur
              instructionsBuff ++= compareMovs(remainingHead, remainingTail)
          }
        case _ =>
          instructionsBuff += cur
          instructionsBuff ++= compareMovs(remainingHead, remainingTail)
      }
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
