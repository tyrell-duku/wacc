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
  /* Translates malloc, realloc and calloc memory allocations into our IR
     instruction set. */
  def transMemoryAlloc(
      t: Type,
      elem: MemoryAlloc,
      freeReg: Reg
  ): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    // For a memory alloc to occur type must be a pointer type
    val PtrT(inner) = t
    elem match {
      case Malloc(size, _) =>
        instructions ++= transExp(size, freeReg)
        instructions += Mov(resultReg, freeReg)
        // Move size into R0 for branch to malloc
        instructions += BranchLink(Label("malloc"))
      case Realloc(ptr, size, _) =>
        instructions ++= transExp(size, freeReg)
        instructions += Mov(R1, freeReg)
        instructions ++= transExp(ptr, freeReg)
        instructions += Mov(R0, freeReg)
        // Move ptr address into R0 and new size to R1 for branch to realloc
        instructions += BranchLink(Label("realloc"))
      case Calloc(num, size, _) =>
        instructions ++= transExp(size, freeReg)
        instructions += Mov(R1, freeReg)
        instructions ++= transExp(num, freeReg)
        instructions += Mov(R0, freeReg)
        // Move number into R0 and size per var to R1 for branch to calloc
        instructions += BranchLink(Label("calloc"))
    }
    instructions += Mov(freeReg, resultReg)
    instructions
  }

  /* Called upon for any pointer arithmetic in Add or Sub cases. Increments
     the pointer by the correct amount (1 or 4 bytes) by checking its type
     and subsequent base type size.  */
  def pointerArith(l: Expr, rd: Reg): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    val t = getExprType(l)
    // Pointer arithmetic should only occur in add/subtract if the lhs is a 
    // pointer.
    if (t.isPtr) {
      val PtrT(inner) = t
      val varSize = getBaseTypeSize(inner)
      // If varSize is greater than 1 byte (typically 4 bytes) then add or
      // subtract by relevant variable size.
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
