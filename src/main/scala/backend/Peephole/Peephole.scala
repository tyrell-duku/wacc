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
import scala.collection.mutable.ListBuffer

object Peephole {

  /* Blocks of instructions that should be ignored */
  val ignoreBlocks = mutable.ListBuffer.empty[Label]

  /* Optimises a block of instructions */
  def optimiseBlock(
      block: (Label, List[Instruction])
  ): (Label, List[Instruction]) = {
    val (label, instructions) = block
    val optimised = mutable.ListBuffer.empty[Instruction]
    val instrs = mutable.ListBuffer.empty[Instruction]
    instrs.addAll(instructions)
    // Store optimised instructions into optimised
    optimise(instrs, optimised)
    (label, optimised.toList)
  }

  /* Continues optimise recursively */
  def continueOptimise(
      cur: Instruction,
      remainingHead: Instruction,
      remainingTail: mutable.ListBuffer[Instruction],
      optimised: mutable.ListBuffer[Instruction]
  ): Unit = {
    optimised += cur
    optimise(remainingHead, remainingTail, optimised)
  }

  /* Overloaded optimise function that is recursively called to store
     optimised instructions in optimised*/
  def optimise(
      instructions: mutable.ListBuffer[Instruction],
      optimised: mutable.ListBuffer[Instruction]
  ): Unit = {
    if (!instructions.isEmpty) {
      optimise(instructions.head, instructions.tail, optimised)
    }
  }

  /* Overloaded optimise function that is recursively called to store
     optimised instructions in optimised*/
  def optimise(
      cur: Instruction,
      instructions: mutable.ListBuffer[Instruction],
      optimised: mutable.ListBuffer[Instruction]
  ): Unit = {
    if (instructions.isEmpty) {
      optimised += cur
    } else {
      val remainingHead = instructions.head
      val remainingTail = instructions.tail
      (cur, remainingHead) match {
        case (Mov(r1, op1), Mov(rd, r2)) =>
          // Remove redundance Mov Instructions
          peepMov(r1, op1, rd, r2, remainingTail, optimised)
        case (Mov(r1, op1), Cmp(rd, op2)) =>
          // Check for redundant compare branches
          if (r1 == rd) {
            peepBranch(op1, op2, remainingHead, remainingTail, optimised)
          } else {
            continueOptimise(cur, remainingHead, remainingTail, optimised)
          }
        case (Ldr(r1, op1), Ldr(r2, op2)) =>
          // Potential strong operation
          peepStrong(r1, op1, r2, op2, remainingTail, optimised)
        case _ =>
          continueOptimise(cur, remainingHead, remainingTail, optimised)
      }
    }
  }

  /* Call optimise on all blocks necessary */
  def optimiseBlocks(
      blocks: List[(Label, List[Instruction])]
  ): List[(Label, List[Instruction])] = {
    val returnBlocks = mutable.ListBuffer.empty[(Label, List[Instruction])]
    for (b <- blocks) {
      val (Label(name), _) = b
      // Ignore blocks that are not required
      if (!ignoreBlocks.contains(Label(name))) {
        name.take(2) match {
          // Not necassary to optimise predefined blocks
          case "p_" => returnBlocks += b
          case _    => returnBlocks += optimiseBlock(b)
        }
      }
    }
    returnBlocks.toList
  }
}
