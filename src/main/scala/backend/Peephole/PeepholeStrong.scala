package backend

import backend.IR.InstructionSet._
import backend.IR.Operand._
import backend.IR.Condition._
import backend.Peephole._

import scala.collection.mutable.ListBuffer
import scala.math._

object PeepholeStrong {

  def log2(i: Int): Double = {
    if (i == 0) {
      -0.1
    } else {
      log(i) / log(2.0)
    }
  }

  def peepholeStrong(
      r1: Reg,
      op1: LoadOperand,
      r2: Reg,
      op2: LoadOperand,
      instructionsBuff: ListBuffer[Instruction],
      remainingTail: ListBuffer[Instruction]
  ): ListBuffer[Instruction] = {
    val operation = remainingTail.head
    if (operation == SMul(r1, r2, r1, r2)) {
      op2 match {
        case ImmMem(n) =>
          val shiftAmount = log2(n)
          if (shiftAmount == floor(shiftAmount)) {
            instructionsBuff += Ldr(r1, op1)

            if (shiftAmount != 0) {
              instructionsBuff += Mov(r1, LSL(r1, ImmInt(shiftAmount.toInt)))
              instructionsBuff += BranchLinkCond(
                VS,
                Label("p_throw_overflow_error")
              )
            }

            instructionsBuff ++= optimise(
              remainingTail.tail.tail.tail.head,
              remainingTail.tail.tail.tail.tail
            )
            return instructionsBuff
          }
        case _ =>
      }

      op1 match {
        case ImmMem(n) =>
          val shiftAmount = log2(n)
          if (shiftAmount == floor(shiftAmount)) {
            instructionsBuff += Ldr(r1, op2)

            if (shiftAmount != 0) {
              instructionsBuff += Mov(r1, LSL(r1, ImmInt(shiftAmount.toInt)))
              instructionsBuff += BranchLinkCond(
                VS,
                Label("p_throw_overflow_error")
              )
            }

            instructionsBuff ++= optimise(
              remainingTail.tail.tail.tail.head,
              remainingTail.tail.tail.tail.tail
            )
            return instructionsBuff
          }
        case _ =>
      }
    } else if (remainingTail.size > 4)
      if (
        remainingTail.tail.tail.tail.head == BranchLink(Label("__aeabi_idiv"))
      ) {
        op2 match {
          case ImmMem(n) =>
            val shiftAmount = log2(n)
            if (shiftAmount == floor(shiftAmount) && shiftAmount != 0) {
              remainingTail.head match {
                case Mov(rd, _) =>
                  instructionsBuff += Ldr(r1, op1)
                  instructionsBuff += Mov(
                    r1,
                    LSR(r1, ImmInt(shiftAmount.toInt))
                  )
                  instructionsBuff += Mov(rd, r1)
                  instructionsBuff ++= optimise(
                    remainingTail.tail.tail.tail.tail.head,
                    remainingTail.tail.tail.tail.tail.tail
                  )
                  return instructionsBuff
                case _ =>
              }
            }
          case _ =>
        }
      }
    instructionsBuff += Ldr(r1, op1)
    instructionsBuff ++= optimise(Ldr(r2, op2), remainingTail)
    instructionsBuff
  }
}
