package backend.CodeGeneration

import backend.CodeGeneration.Expressions.transExp
import backend.CodeGenerator._
import backend.DefinedFuncs.PreDefinedFuncs.{ArrayBounds, addRuntimeError}
import backend.DefinedFuncs.ReadInstructions.{charRead, intRead}
import backend.IR.InstructionSet._
import backend.IR.Operand._
import frontend.Rules._

import scala.collection.mutable.ListBuffer

object Arrays {

  def loadArrayElem(
      id: Ident,
      es: List[Expr],
      reg: Reg
  ): ListBuffer[Instruction] = {
    val (isByte, instructions) = transArrayElem(id, es, reg)

    instructions += Ldr(isByte, reg, reg, 0)

  }

  def storeArrayElem(
      id: Ident,
      es: List[Expr],
      reg: Reg
  ): ListBuffer[Instruction] = {
    val freeReg = getFreeReg()
    val (isByte, instructions) = transArrayElem(id, es, freeReg)

    instructions += Str(isByte, reg, freeReg, 0)

    addUnusedReg(freeReg)
    instructions
  }

  def transArrayElem(
      id: Ident,
      es: List[Expr],
      reg: Reg
  ): (Boolean, ListBuffer[Instruction]) = {
    var (index, t) = sTable(id)
    val instructions = ListBuffer.empty[Instruction]

    val baseTypeSize = getBaseTypeSize(t)
    val spOffset = currentSP - index
    instructions += backend.IR.InstructionSet.Add(reg, SP, ImmInt(spOffset))
    val nextReg = getFreeReg()
    for (exp <- es) {
      t = getInnerType(t)
      instructions ++= transExp(exp, nextReg)
      instructions += Ldr(reg, RegAdd(reg))

      // Values must be in R0 & R1 for branch
      instructions += Mov(R0, nextReg)
      instructions += Mov(R1, reg)
      instructions += BranchLink(addRuntimeError(ArrayBounds))

      instructions += Add(reg, reg, ImmInt(INT_SIZE))

      val isSizeByte = isByte(t)
      if (isSizeByte) {
        instructions += Add(reg, reg, nextReg)
      } else {
        instructions += Add(reg, reg, LSL(nextReg, ImmInt(2)))
      }
    }
    addUnusedReg(nextReg)
    (isByte(t), instructions)
  }

}
