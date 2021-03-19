package backend.CodeGeneration

import scala.collection.mutable.ListBuffer
import backend.CodeGeneration.Expressions.transExp
import backend.CodeGenerator._
import backend.DefinedFuncs.PrintInstrs.stringPrintInstrs
import backend.DefinedFuncs.PreDefinedFuncs.{Overflow, RuntimeError}
import backend.DefinedFuncs.RuntimeErrors.addRuntimeError
import backend.IR.InstructionSet._
import backend.IR.Operand._
import backend.IR.Condition.{NE, VS}
import frontend.Rules._
import scala.collection.immutable.Map
import scala.collection.immutable.Set

object MemoryAllocs {
  // HashMap for allocated memory
  private var userHeap = Map.empty[Ident, Int]
  // Set of unique pseudo addresses currently in use
  private var addressSet = Set.empty[Int]
  // Next unique identifer for pseudo addresses
  private var nextAddress = 0
  // Constant needed for overflow check
  private val Overflow_Right_Shift = 31
  // Free runtime error messages
  private val doubleFreeError =
    "FreeError: unable to free memory that has been previously freed."
  private val unallocatedMemError =
    "FreeError: unable to free unallocated memory."

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
        // Move size into R0 for branch to malloc
        instructions += Mov(resultReg, freeReg)
        instructions += BranchLink(Label("malloc"))
      case Realloc(ptr, size, _) =>
        instructions ++= transExp(size, freeReg)
        // Move ptr address into R0 and new size to R1 for branch to realloc
        instructions += Mov(R1, freeReg)
        instructions ++= transExp(ptr, freeReg)
        instructions += Mov(resultReg, freeReg)
        instructions += BranchLink(Label("realloc"))
      case Calloc(num, size, _) =>
        instructions ++= transExp(size, freeReg)
        // Move number into R0 and size per var to R1 for branch to calloc
        instructions += Mov(R1, freeReg)
        instructions ++= transExp(num, freeReg)
        instructions += Mov(resultReg, freeReg)
        instructions += BranchLink(Label("calloc"))
    }
    instructions += Mov(freeReg, resultReg)
    instructions
  }

  /* Called upon for any pointer arithmetic in Add or Sub cases. */
  def pointerArith(l: Expr, rd: Reg): ListBuffer[Instruction] = {
    // Pointer arithmetic should only occur in add/subtract if the lhs is a
    // pointer.
    getExprType(l) match {
      case t: PtrT => pointerArithOffset(t, rd)
      case _       => ListBuffer.empty[Instruction]
    }
  }

  /* Increments the pointer by the correct amount (1 or 4 bytes) by checking
     its type  and subsequent base type size.  */
  private def pointerArithOffset(t: Type, rd: Reg) = {
    val instructions = ListBuffer.empty[Instruction]
    val PtrT(inner) = t
    val varSize = getBaseTypeSize(inner)
    // If varSize is greater than 1 byte (typically 4 bytes) then add or
    // subtract by relevant variable size.
    if (varSize > CHAR_SIZE) {
      val rm = getFreeReg()
      instructions += Ldr(rm, ImmMem(varSize))
      // Runtime error check
      instructions += SMul(rd, rm, rd, rm)
      instructions += Cmp(rm, ASR(rd, ImmInt(Overflow_Right_Shift)))
      addUnusedReg(rm)
      instructions += BranchLinkCond(NE, addRuntimeError(Overflow))
    }
    instructions
  }

  /* Loads the element from the requested pointer elem into REG.
     Function is called in transArrayElem and therefore handles nested
     array or pointer cases. */
  def transPointerElem(t: Type, elemExp: Expr, reg: Reg, nextReg: Reg) = {
    val instructions = ListBuffer.empty[Instruction]
    instructions += Ldr(reg, RegAdd(reg))
    instructions ++= transExp(elemExp, nextReg)
    instructions ++= pointerArithOffset(t, nextReg)
    instructions += Add(reg, reg, nextReg)
  }

  /* Returns the next unique psuedo address to use and increments the
     counter. */
  private def getNextAddress: Int = {
    nextAddress += 1
    nextAddress
  }

  /* Add ident for new pointer to heap map with corresponding psuedo address.
     Updates pointer re-assignments to point to the same psuedo address. */
  def addToHeap(id: Ident, rhs: AssignRHS): Unit = rhs match {
    case rightId: Ident =>
      userHeap += ((id, userHeap(rightId)))
    case memAlloc: MemoryAlloc =>
      val nextAddr = getNextAddress
      userHeap += ((id, nextAddr))
      addressSet += nextAddr
    case _ =>
  }

  /* Returns IR representation to print free error message and return a runtime
     error. */
  private def printFreeError(msg: String): ListBuffer[Instruction] = {
    val msgLabel = dataTable.addDataEntry(msg)
    ListBuffer(
      Ldr(resultReg, DataLabel(msgLabel)),
      BranchLink(addRuntimeError(RuntimeError))
    )
  }

  /* Frees a pointer if and only if it has been previously allocated (defined
     in the heap). Otherwise it throws a runtime error if it has been
     previously freed or is trying to free unallocated memory. */
  def freePointer(id: Ident): ListBuffer[Instruction] = {
    val addr = userHeap(id)
    if (addressSet(addr)) {
      addressSet -= addr
      ListBuffer(BranchLink(Label("free")))
    } else {
      printFreeError(doubleFreeError)
    }
  }

  /* Called in transFree if free(&x) occurs where x is a variable on the
     stack.*/
  def throwUnallocatedMemError: ListBuffer[Instruction] = {
    printFreeError(unallocatedMemError)
  }
}
