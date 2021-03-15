package backend.CodeGeneration

import scala.collection.mutable.ListBuffer
import backend.CodeGeneration.Expressions.transExp
import backend.CodeGenerator._
import backend.DefinedFuncs.PreDefinedFuncs.Overflow
import backend.DefinedFuncs.RuntimeErrors.addRuntimeError
import backend.IR.InstructionSet._
import backend.IR.Operand._
import backend.IR.Condition.{NE, VS}
import frontend.Rules._

object MemoryAllocs {
  def transMemoryAlloc(
      t: Type,
      elem: MemoryAlloc,
      freeReg: Reg
  ): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    val PtrT(inner) = t
    val varSize = getBaseTypeSize(inner)
    elem match {
      case Malloc(size, _) =>
        instructions ++= transExp(size, freeReg)
        instructions += Mov(resultReg, freeReg)
        instructions += BranchLink(Label("malloc"))
      case Realloc(ptr, size, _) =>
        instructions ++= transExp(size, freeReg)
        instructions += Mov(R1, freeReg)
        instructions ++= transExp(ptr, freeReg)
        instructions += Mov(R0, freeReg)
        instructions += BranchLink(Label("realloc"))
      case Calloc(num, size, _) =>
        instructions ++= transExp(size, freeReg)
        instructions += Mov(R1, freeReg)
        instructions ++= transExp(num, freeReg)
        instructions += Mov(R0, freeReg)
        instructions += BranchLink(Label("calloc"))
    }
    instructions += Mov(freeReg, resultReg)
    instructions
  }

  def pointerArith(l: Expr, rd: Reg): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    val t = getExprType(l)

    if (t.isPtr) {
      val PtrT(inner) = t
      val varSize = getBaseTypeSize(inner)
      if (varSize > CHAR_SIZE) {
        val rm = getFreeReg()
        instructions += Ldr(rm, ImmMem(varSize))
        // Runtime error check
        instructions += SMul(rd, rm, rd, rm)
        instructions += Cmp(rm, ASR(rd, ImmInt(31)))
        addUnusedReg(rm)
        instructions += BranchLinkCond(NE, addRuntimeError(Overflow))
      }

    }
    instructions
  }
}
