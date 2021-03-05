package backend.CodeGeneration

import backend.CodeGenerator._
import backend.DefinedFuncs.PreDefinedFuncs._
import backend.IR.InstructionSet._
import backend.IR.Operand._
import frontend.Rules.{ArrayT, Ident, Pair}

import scala.collection.mutable.ListBuffer

object Free {

  def transFree(id: Ident): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    val freeReg = getFreeReg()
    val (index, t) = sTable(id)
    instructions += Ldr(freeReg, SP, currentSP - index)
    instructions += Mov(R0, freeReg)
    addUnusedReg(freeReg)
    t match {
      case Pair(_, _) =>
        instructions += BranchLink(addRuntimeError(FreePair))

      case ArrayT(_) =>
        instructions += BranchLink(addRuntimeError(FreeArray))

      // Semantically incorrect
      case _ => ListBuffer.empty[Instruction]
    }
    instructions

  }
}
