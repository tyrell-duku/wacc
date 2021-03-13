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

  /* Get log2 of i, returns LOG_ERROR if I == 0 or value is not an Integer */
  def getShiftAmount(i: Int): Int = {
    if (i <= 0) {
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

  /* Reduces strength of a division if possible */
  def divisionReduc(
      r1: Reg,
      op1: LoadOperand,
      r2: Reg,
      op2: LoadOperand,
      instructions: mutable.ListBuffer[Instruction],
      optimised: mutable.ListBuffer[Instruction]
  ): Unit = {
    val load1 = Ldr(r1, op1)
    val load2 = Ldr(r2, op2)
    op2 match {
      // If dividing by 0, call DIVIDE_BY_ZERO runtime error
      case ImmMem(0) =>
        // Call DIVIDE_BY_ZERO runtime error
        optimised += Ldr(
          R0,
          DataLabel(Label("msg_divide_by_zero"))
        )
        optimised += BranchLink(RuntimeError.funcLabel)
      case ImmMem(n) =>
        val shiftAmount = getShiftAmount(n)
        op1 match {
          case ImmMem(n1) =>
            if ((shiftAmount != LOG_ERROR && getShiftAmount(n1) != LOG_ERROR)) {
              instructions.head match {
                // Get the destination reg for the division
                case Mov(rd, _) =>
                  // Instructions in order to shift rather than using __aeabi_idiv
                  val newInstructions = instructions.drop(4)
                  // Cons Instructions to the head of the list
                  Mov(rd, r1) +=: newInstructions
                  if (shiftAmount != 0) {
                    // Add(r1, r1, r2) +=: newInstructions
                    Mov(r1, LSR(r1, ImmInt(shiftAmount))) +=: newInstructions
                  }
                  // Mov(r2, ASR(r1, ImmInt(31))) +=: newInstructions
                  load1 +=: newInstructions
                  // Optimise instructinos from NEWINSTRUCTIONS
                  optimise(newInstructions, optimised)
                case _ =>
                  continueOptimise(load1, load2, instructions, optimised)
              }
            } else {
              continueOptimise(load1, load2, instructions, optimised)
            }
          case _ => continueOptimise(load1, load2, instructions, optimised)
        }
      case _ =>
        continueOptimise(load1, load2, instructions, optimised)
    }
  }

  /* Reduces strength of a multiplication if possible */
  def multiplyReduc(
      r1: Reg,
      op1: LoadOperand,
      r2: Reg,
      op2: LoadOperand,
      instructions: mutable.ListBuffer[Instruction],
      optimised: mutable.ListBuffer[Instruction]
  ): Unit = {
    val load1 = Ldr(r1, op1)
    val load2 = Ldr(r2, op2)
    // Ensure SMULL instruction
    if (instructions.size >= 4) {
      var newInstructions = instructions.drop(3)
      (op1, op2) match {
        // If multiplying by 0, load 0 into the destination register
        case (ImmMem(0), _) | (_, ImmMem(0)) =>
          optimise(Ldr(r1, ImmMem(0)), instructions.drop(3), optimised)
        case (ImmMem(n1), ImmMem(n2)) =>
          val shiftAmount1 = getShiftAmount(n1)
          val shiftAmount2 = getShiftAmount(n2)
          // Get largest shift to optimise division
          if (shiftAmount1 > shiftAmount2 && shiftAmount1 != LOG_ERROR) {
            shiftOptimise(shiftAmount1, instructions, r1, r2, op2, optimised)
          } else if (
            shiftAmount1 <= shiftAmount2 && shiftAmount2 != LOG_ERROR
          ) {
            shiftOptimise(shiftAmount2, instructions, r1, r2, op1, optimised)
          } else {
            continueOptimise(load1, load2, instructions, optimised)
          }
        case _ =>
          continueOptimise(load1, load2, instructions, optimised)
      }
    } else {
      continueOptimise(load1, load2, instructions, optimised)
    }
  }

  // Adds shift instuctions and continues to optimise
  def shiftOptimise(
      shiftAmount: Int,
      instructions: mutable.ListBuffer[Instruction],
      r1: Reg,
      r2: Reg,
      op1: LoadOperand,
      optimised: mutable.ListBuffer[Instruction]
  ): Unit = {
    var newInstructions = instructions.drop(3)
    if (shiftAmount != 0) {
      newInstructions = instructions.tail
      Mov(r1, ASL(r1, ImmInt(shiftAmount))) +=: newInstructions
      Mov(r2, ASR(r1, ImmInt(31))) +=: newInstructions
    }
    Ldr(r1, op1) +=: newInstructions

    // Optimise instructinos from NEWINSTRUCTIONS
    optimise(newInstructions, optimised)
  }

  /* Function for strength reduction */
  def peepStrong(
      r1: Reg,
      op1: LoadOperand,
      r2: Reg,
      op2: LoadOperand,
      instructions: mutable.ListBuffer[Instruction],
      optimised: mutable.ListBuffer[Instruction]
  ): Unit = {
    val load1 = Ldr(r1, op1)
    val load2 = Ldr(r2, op2)
    // Multiplication instruction
    if (instructions.head == SMul(r1, r2, r1, r2)) {
      multiplyReduc(r1, op1, r2, op2, instructions, optimised)
    } else if (instructions.size >= 4) {
      // Division instruction
      if (instructions.drop(3).head == BranchLink(Label("__aeabi_idiv"))) {
        divisionReduc(r1, op1, r2, op2, instructions, optimised)
      } else {
        continueOptimise(load1, load2, instructions, optimised)
      }
    } else {
      continueOptimise(load1, load2, instructions, optimised)
    }
  }
}
