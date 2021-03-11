package backend

import backend.IR.InstructionSet._
import backend.IR.Operand._
import backend.IR.Condition._
import backend.DefinedFuncs.PreDefinedFuncs._
import backend.Peephole._
import scala.collection._
import scala.math._

object PeepholeStrong {

  val LOG_ERROR = -1

  def getShiftAmount(i: Int): Int = {
    if (i == 0) {
      LOG_ERROR
    } else {
      val dVal = log(i) / log(2.0)
      if (dVal == floor(dVal)) {
        dVal.toInt
      } else {
        LOG_ERROR
      }
    }
  }

  def divisionReduction(
      r1: Reg,
      op1: LoadOperand,
      r2: Reg,
      op2: LoadOperand,
      instructionsBuff: mutable.ListBuffer[Instruction],
      remainingTail: mutable.ListBuffer[Instruction]
  ): mutable.ListBuffer[Instruction] = {
    op2 match {
      case ImmMem(0) =>
        remainingTail.tail.head match {
          case Mov(rd, r3) =>
            if (r3 == r2) {
              instructionsBuff += Ldr(
                R0,
                DataLabel(Label("msg_divide_by_zero"))
              )
              instructionsBuff += BranchLink(RuntimeError.funcLabel)
              return instructionsBuff
            }
          case _ =>
        }
      case ImmMem(n) =>
        val shiftAmount = getShiftAmount(n)
        if (!(shiftAmount == LOG_ERROR)) {
          remainingTail.head match {
            case Mov(rd, _) =>
              instructionsBuff += Ldr(r1, op1)
              if (shiftAmount != 0) {
                instructionsBuff += Mov(
                  r1,
                  LSR(r1, ImmInt(shiftAmount))
                )
              }
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
    instructionsBuff += Ldr(r1, op1)
    instructionsBuff ++= optimise(Ldr(r2, op2), remainingTail)
    instructionsBuff
  }

  def multiplyReduction(
      r1: Reg,
      op1: LoadOperand,
      r2: Reg,
      op2: LoadOperand,
      instructionsBuff: mutable.ListBuffer[Instruction],
      remainingTail: mutable.ListBuffer[Instruction]
  ): mutable.ListBuffer[Instruction] = {
    if (remainingTail.size >= 4) {
      (op1, op2) match {
        case (ImmMem(n1), ImmMem(n2)) =>
          val shiftAmount1 = getShiftAmount(n1)
          val shiftAmount2 = getShiftAmount(n2)
          if (shiftAmount1 > shiftAmount2 && shiftAmount1 != LOG_ERROR) {
            instructionsBuff += Ldr(r1, op2)
            if (shiftAmount1 != 0) {
              instructionsBuff += Mov(r1, LSL(r1, ImmInt(shiftAmount1)))
            }
            instructionsBuff ++= optimise(
              remainingTail.tail.tail.tail.head,
              remainingTail.tail.tail.tail.tail
            )
            return instructionsBuff
          } else if (
            shiftAmount1 <= shiftAmount2 && shiftAmount2 != LOG_ERROR
          ) {
            instructionsBuff += Ldr(r1, op1)
            if (shiftAmount2 != 0) {
              instructionsBuff += Mov(r1, LSL(r1, ImmInt(shiftAmount2)))
            }
            instructionsBuff ++= optimise(
              remainingTail.tail.tail.tail.head,
              remainingTail.tail.tail.tail.tail
            )
            return instructionsBuff
          }
        case _ =>
      }
    }
    instructionsBuff += Ldr(r1, op1)
    instructionsBuff ++= optimise(Ldr(r2, op2), remainingTail)
    instructionsBuff
  }

  def peepholeStrong(
      r1: Reg,
      op1: LoadOperand,
      r2: Reg,
      op2: LoadOperand,
      instructionsBuff: mutable.ListBuffer[Instruction],
      remainingTail: mutable.ListBuffer[Instruction]
  ): mutable.ListBuffer[Instruction] = {
    if (remainingTail.head == SMul(r1, r2, r1, r2)) {
      return multiplyReduction(
        r1,
        op1,
        r2,
        op2,
        instructionsBuff,
        remainingTail
      )
    } else if (remainingTail.size >= 4)
      if (
        remainingTail.tail.tail.tail.head == BranchLink(Label("__aeabi_idiv"))
      ) {
        return divisionReduction(
          r1,
          op1,
          r2,
          op2,
          instructionsBuff,
          remainingTail
        );
      }
    instructionsBuff += Ldr(r1, op1)
    instructionsBuff ++= optimise(Ldr(r2, op2), remainingTail)
    instructionsBuff
  }
}
