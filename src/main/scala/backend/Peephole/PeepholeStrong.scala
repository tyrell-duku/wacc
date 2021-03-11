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
      instructions: mutable.ListBuffer[Instruction],
      optimised: mutable.ListBuffer[Instruction]
  ): Unit = {
    op2 match {
      case ImmMem(0) =>
        instructions.tail.head match {
          case Mov(rd, r3) =>
            if (r3 == r2) {
              optimised += Ldr(
                R0,
                DataLabel(Label("msg_divide_by_zero"))
              )
              optimised += BranchLink(RuntimeError.funcLabel)
              return
            } else {
              continueOptimise(
                Ldr(r1, op1),
                Ldr(r2, op2),
                instructions,
                optimised
              )
            }
          case _ =>
            continueOptimise(
              Ldr(r1, op1),
              Ldr(r2, op2),
              instructions,
              optimised
            )
        }
      case ImmMem(n) =>
        val shiftAmount = getShiftAmount(n)
        if (!(shiftAmount == LOG_ERROR)) {
          val newInstructions = instructions.drop(4)
          if (shiftAmount != 0) {
            Mov(r1, LSR(r1, ImmInt(shiftAmount))) +=: newInstructions
          }
          Ldr(r1, op1) +=: newInstructions
          optimise(newInstructions, optimised)
        } else {
          continueOptimise(Ldr(r1, op1), Ldr(r2, op2), instructions, optimised)
        }
      case _ =>
        continueOptimise(Ldr(r1, op1), Ldr(r2, op2), instructions, optimised)
    }
  }

  def multiplyReduction(
      r1: Reg,
      op1: LoadOperand,
      r2: Reg,
      op2: LoadOperand,
      instructions: mutable.ListBuffer[Instruction],
      optimised: mutable.ListBuffer[Instruction]
  ): Unit = {
    if (instructions.size >= 4) {
      (op1, op2) match {
        case (ImmMem(0), _) | (_, ImmMem(0)) =>
          optimise(Ldr(r1, ImmMem(0)), instructions.tail.tail.tail, optimised)
        case (ImmMem(n1), ImmMem(n2)) =>
          val shiftAmount1 = getShiftAmount(n1)
          val shiftAmount2 = getShiftAmount(n2)
          if (shiftAmount1 > shiftAmount2 && shiftAmount1 != LOG_ERROR) {
            val newInstructions = instructions.drop(3)
            if (shiftAmount1 != 0) {
              Mov(r1, LSL(r1, ImmInt(shiftAmount1))) +=: newInstructions
            }
            Ldr(r1, op2) +=: newInstructions
            optimise(newInstructions, optimised)
          } else if (
            shiftAmount1 <= shiftAmount2 && shiftAmount2 != LOG_ERROR
          ) {
            val newInstructions = instructions.drop(3)
            if (shiftAmount2 != 0) {
              Mov(r1, LSL(r1, ImmInt(shiftAmount2))) +=: newInstructions
            }
            Ldr(r1, op1) +=: newInstructions
            optimise(newInstructions, optimised)
          } else {
            continueOptimise(
              Ldr(r1, op1),
              Ldr(r2, op2),
              instructions,
              optimised
            )
          }
        case _ =>
          continueOptimise(Ldr(r1, op1), Ldr(r2, op2), instructions, optimised)
      }
    } else {
      continueOptimise(Ldr(r1, op1), Ldr(r2, op2), instructions, optimised)
    }
  }

  def peepholeStrong(
      r1: Reg,
      op1: LoadOperand,
      r2: Reg,
      op2: LoadOperand,
      instructions: mutable.ListBuffer[Instruction],
      optimised: mutable.ListBuffer[Instruction]
  ): Unit = {
    if (instructions.head == SMul(r1, r2, r1, r2)) {
      multiplyReduction(
        r1,
        op1,
        r2,
        op2,
        instructions,
        optimised
      )
      return
    } else if (instructions.size >= 4) {
      if (
        instructions.tail.tail.tail.head == BranchLink(Label("__aeabi_idiv"))
      ) {
        divisionReduction(
          r1,
          op1,
          r2,
          op2,
          instructions,
          optimised
        )
        return
      }
    }
    continueOptimise(Ldr(r1, op1), Ldr(r2, op2), instructions, optimised)
  }
}
