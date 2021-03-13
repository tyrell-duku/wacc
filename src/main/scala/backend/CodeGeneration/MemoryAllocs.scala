package backend.CodeGeneration

import scala.collection.mutable.ListBuffer
import backend.CodeGeneration.Expressions.transExp
import backend.CodeGenerator._
import backend.IR.InstructionSet._
import backend.IR.Operand._
import frontend.Rules._

object MemoryAllocs {
  def transMemoryAlloc(
      t: Type,
      elem: MemoryAlloc,
      freeReg: Reg
  ): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    elem match {
      case Malloc(size, _) =>
        val PtrT(inner) = t
        val varSize = getBaseTypeSize(inner)
        val arraySize =
          Plus(
            Div(size, IntLiter(varSize, null), null),
            IntLiter(INT_SIZE, null),
            null
          )
        instructions ++= transExp(arraySize, freeReg)
        val nextFreeReg = getFreeReg()
        instructions += Mov(nextFreeReg, freeReg)
        instructions += Mov(resultReg, freeReg)
        instructions += BranchLink(Label("malloc"))
        instructions += Str(nextFreeReg, RegAdd(freeReg))
      case Realloc(ptr, size, _) =>
      case Calloc(num, size, _)  =>
    }
    instructions
  }
}
