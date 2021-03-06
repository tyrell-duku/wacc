package backend.CodeGeneration

import backend.CodeGeneration.Expressions.transExp
import backend.CodeGeneration.MemoryAllocs.transPointerElem
import backend.CodeGenerator._
import backend.DefinedFuncs.PreDefinedFuncs.ArrayBounds
import backend.DefinedFuncs.RuntimeErrors.addRuntimeError
import backend.IR.InstructionSet._
import backend.IR.Operand._
import frontend.Rules._

import scala.collection.mutable.ListBuffer

object Arrays {
  private val SHIFT_TWO = 2

  /* Loads an array elem into register Reg. */
  def loadArrayElem(
      id: Ident,
      es: List[Expr],
      reg: Reg
  ): ListBuffer[Instruction] = {
    val (isByte, instructions) = transArrayElem(id, es, reg)
    instructions += Ldr(isByte, reg, reg, NO_OFFSET)
  }

  /* Stores an expression from Reg into the array elem. */
  def storeArrayElem(
      id: Ident,
      es: List[Expr],
      reg: Reg
  ): ListBuffer[Instruction] = {
    val freeReg = getFreeReg()
    val (isByte, instructions) = transArrayElem(id, es, freeReg)
    instructions += Str(isByte, reg, freeReg, NO_OFFSET)
    addUnusedReg(freeReg)
    instructions
  }

  /* Translates an array elem to the internal representation. */
  def transArrayElem(
      id: Ident,
      es: List[Expr],
      reg: Reg
  ): (Boolean, ListBuffer[Instruction]) = {
    val instructions = ListBuffer.empty[Instruction]
    var (index, t) = sTable(id)
    val baseTypeSize = getBaseTypeSize(t)
    val spOffset = currentSP - index
    // Gets the SP address where the address for the array is stored
    instructions += Add(reg, SP, ImmInt(spOffset))
    val nextReg = getFreeReg()
    // Handles nested array elems
    for (exp <- es) {
      if (t.isPtr) {
        instructions ++= transPointerElem(t, exp, reg, nextReg)
      } else {
        // Gets type of array elem at current depth
        t = getInnerType(t)
        instructions ++= transExp(exp, nextReg)
        instructions += Ldr(reg, RegAdd(reg))
        // Values must be in R0 & R1 for array bounds check
        instructions += Mov(resultReg, nextReg)
        instructions += Mov(R1, reg)
        instructions += BranchLink(addRuntimeError(ArrayBounds))
        // Add offset to account for array size at start of array in memory
        instructions += Add(reg, reg, ImmInt(INT_SIZE))
        // Gets address of array elem
        if (isByte(t)) {
          instructions += Add(reg, reg, nextReg)
        } else {
          // LSL to account for 4 byte increment between elems
          instructions += Add(reg, reg, LSL(nextReg, ImmInt(SHIFT_TWO)))
        }
      }
    }
    addUnusedReg(nextReg)
    (isByte(t), instructions)
  }

  /* Translates an array literal into the internal representation. */
  def transArrayLiter(
      t: Type,
      opArr: Option[List[Expr]],
      freeReg: Reg
  ): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    val ArrayT(innerType) = t
    val arr = opArr match {
      case Some(arr) => arr
      case None      => List.empty[Expr]
    }
    val listSize = arr.size
    // Size needed to store all elems of array + array size in memory
    instructions += Ldr(
      resultReg,
      ImmMem(INT_SIZE + (listSize * getBaseTypeSize(innerType)))
    )
    instructions += BranchLink(Label("malloc"))
    // Malloc size must be in R0 for "malloc"
    instructions += Mov(freeReg, resultReg)
    val nextFreeReg = getFreeReg()
    val typeSizeIsByte = isByte(innerType)
    // If array elems are of size byte, offset increments by 1 for each
    // elem, otherwise increments by 4
    val offset: (Int => Int) =
      if (typeSizeIsByte) (i => i + INT_SIZE)
      else (i => (i + 1) * ADDRESS_SIZE)
    // Store all array elements into memory
    for (i <- 0 until listSize) {
      instructions ++= transExp(arr(i), nextFreeReg)
      instructions += Str(typeSizeIsByte, nextFreeReg, freeReg, offset(i))
    }
    // Store array size into memory at start of malloced memory
    instructions += Ldr(nextFreeReg, ImmMem(listSize))
    instructions += Str(nextFreeReg, RegAdd(freeReg))
    addUnusedReg(nextFreeReg)
    instructions
  }

}
