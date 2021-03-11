package backend

import java.io.{File, FileWriter}
import backend.IR.InstructionSet._
import backend.IR.Operand._
import backend.IR.Condition._
import backend.PeepholeMov._
import backend.PeepholeBranch._
import backend.PeepholeStrong._
import backend.DefinedFuncs.PreDefinedFuncs._
import scala.collection._

object Peephole {

  val predefinedFuncs = mutable.ListBuffer(
    // All predefined functions should not be optimised
    ArrayBounds.funcLabel,
    DivideByZero.funcLabel,
    Overflow.funcLabel,
    FreePair.funcLabel,
    FreeArray.funcLabel,
    NullPointer.funcLabel,
    RuntimeError.funcLabel,
    PrintInt.funcLabel,
    PrintBool.funcLabel,
    PrintString.funcLabel,
    PrintReference.funcLabel,
    PrintLn.funcLabel,
    ReadInt.funcLabel,
    ReadInt.funcLabel
  )

  /* Blocks of instructions that should be ignored */
  val ignoreBlocks = mutable.ListBuffer.empty[Label]

  def optimiseBlock(
      block: (Label, List[Instruction])
  ): (Label, List[Instruction]) = {
    val (label, instructions) = block

    val remaining = mutable.ListBuffer.empty[Instruction]
    remaining.addAll(instructions)
    var instructionsBuff = mutable.ListBuffer.empty[Instruction]

    if (!instructions.isEmpty) {
      instructionsBuff = optimise(instructions(0), remaining.tail)
    }

    (label, instructionsBuff.toList)
  }

  def optimise(
      cur: Instruction,
      remaining: mutable.ListBuffer[Instruction]
  ): mutable.ListBuffer[Instruction] = {
    val instructionsBuff = mutable.ListBuffer.empty[Instruction]
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
                return peepholeMov(op1, rd, instructionsBuff, remainingTail)
              }
            case Cmp(rd, op2) =>
              if (r1 == rd) {
                return peepholeBranch(
                  op1,
                  op2,
                  instructionsBuff,
                  remainingHead,
                  remainingTail
                )
              }
            case _ =>
          }
        case Ldr(r1, op1) =>
          remainingHead match {
            case Ldr(r2, op2) =>
              return peepholeStrong(
                r1,
                op1,
                r2,
                op2,
                instructionsBuff,
                remainingTail
              )
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
    val returnBlocks = mutable.ListBuffer.empty[(Label, List[Instruction])]
    for (b <- blocks) {
      val (label, _) = b
      if (!ignoreBlocks.contains(label)) {
        if (predefinedFuncs.contains(label)) {
          returnBlocks += b
        } else {
          returnBlocks += optimiseBlock(b)
        }
      }
    }
    returnBlocks.toList
  }
}
