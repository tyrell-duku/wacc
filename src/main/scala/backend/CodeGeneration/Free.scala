package backend.CodeGeneration

import backend.CodeGenerator._
import backend.DefinedFuncs.PreDefinedFuncs.{FreePair, FreeArray}
import backend.DefinedFuncs.RuntimeErrors.addRuntimeError
import backend.IR.InstructionSet._
import backend.IR.Operand._
import frontend.Rules.{ArrayT, Ident, Pair}
import scala.collection.mutable.ListBuffer

object Free {
  /* Translates free statement to our internal representation.
     Can only free a pair or array, semantically only free(Ident)
     is valid. */
  def transFree(id: Ident): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    val freeReg = getFreeReg()
    val (index, t) = sTable(id)
    // Load variable from memory into freeReg
    instructions += Ldr(freeReg, SP, currentSP - index)
    // Move to R0 for calling pre-defined free function
    instructions += Mov(resultReg, freeReg)
    addUnusedReg(freeReg)
    t match {
      case _: Pair =>
        instructions += BranchLink(addRuntimeError(FreePair))
      case _: ArrayT =>
        instructions += BranchLink(addRuntimeError(FreeArray))
      // Semantically incorrect
      case _ => ListBuffer.empty[Instruction]
    }
    instructions
  }
}
