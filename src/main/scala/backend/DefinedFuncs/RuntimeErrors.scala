package backend.DefinedFuncs

import backend.CodeGenerator.{
  dataTable,
  funcTable,
  resultReg,
  FALSE_INT,
  ADDRESS_SIZE
}
import backend.DefinedFuncs.PreDefinedFuncs._
import backend.DefinedFuncs.PrintInstrs.stringPrintInstrs
import backend.IR.InstructionSet._
import backend.IR.Operand._
import scala.collection.mutable.ListBuffer
import backend.IR.Condition._

object RuntimeErrors {

  private val ERROR_EXIT_CODE = -1

  /* Adds a runtime error ERR to the function table. */
  def addRuntimeError(err: PreDefFunc): Label = {
    funcTable.addEntry(RuntimeError.func)
    funcTable.addEntry(stringPrintInstrs)
    dataTable.addDataEntryWithLabel("msg_string", "%.*s\\0")
    for (i <- 0 until err.msgs.length) {
      dataTable.addDataEntryWithLabel(err.msgName(i), err.msgs(i))
    }
    funcTable.addEntry(err.func)
    err.funcLabel
  }

  /* Throws a runtime error by adding the appropiate instructions to the
     internal representation. */
  def throwRuntimeError: (Label, List[Instruction]) = {
    (
      RuntimeError.funcLabel,
      List[Instruction](
        BranchLink(Label("p_print_string")),
        Mov(resultReg, ImmInt(ERROR_EXIT_CODE)),
        BranchLink(Label("exit"))
      )
    )
  }

  /* Checks the bounds of a given array, branching to a runtime error if
     necessary. */
  def checkArrayBounds: (Label, List[Instruction]) = {
    (
      ArrayBounds.funcLabel,
      List[Instruction](
        Push(ListBuffer(LR)),
        Cmp(resultReg, ImmInt(FALSE_INT)),
        LdrCond(LT, resultReg, DataLabel(Label(ArrayBounds.msgName(0)))),
        BranchLinkCond(LT, RuntimeError.funcLabel),
        Ldr(R1, RegAdd(R1)),
        Cmp(resultReg, R1),
        LdrCond(CS, resultReg, DataLabel(Label(ArrayBounds.msgName(1)))),
        BranchLinkCond(CS, RuntimeError.funcLabel),
        Pop(ListBuffer(PC))
      )
    )
  }

  /* Checks an expression to determine whether an expression has been divided
     by 0, branching to a runtime error if necessary. */
  def checkDivideByZero: (Label, List[Instruction]) = {
    (
      DivideByZero.funcLabel,
      List[Instruction](
        Push(ListBuffer(LR)),
        Cmp(R1, ImmInt(FALSE_INT)),
        LdrCond(EQ, resultReg, DataLabel(Label(DivideByZero.msgName(0)))),
        BranchLinkCond(EQ, RuntimeError.funcLabel),
        Pop(ListBuffer(PC))
      )
    )
  }

  /* Checks an expression to determine whether an int has overflown, branching
     to a runtime error if necessary. */
  def throwOverflowError: (Label, List[Instruction]) = {
    (
      Overflow.funcLabel,
      List[Instruction](
        Ldr(resultReg, DataLabel(Label(Overflow.msgName(0)))),
        BranchLink(RuntimeError.funcLabel)
      )
    )
  }

  /* Free a given pair, branching to a runtime error if necessary. */
  def freePair: (Label, List[Instruction]) = {
    (
      FreePair.funcLabel,
      List[Instruction](
        Push(ListBuffer(LR)),
        Cmp(resultReg, ImmInt(FALSE_INT)),
        LdrCond(EQ, resultReg, DataLabel(Label(FreePair.msgName(0)))),
        BranchCond(EQ, RuntimeError.funcLabel),
        Push(ListBuffer(resultReg)),
        Ldr(resultReg, RegAdd(resultReg)),
        BranchLink(Label("free")),
        Ldr(resultReg, RegAdd(SP)),
        Ldr(resultReg, RegisterOffset(resultReg, ADDRESS_SIZE)),
        BranchLink(Label("free")),
        Pop(ListBuffer(resultReg)),
        BranchLink(Label("free")),
        Pop(ListBuffer(PC))
      )
    )
  }

  /* Free a given array, branching to a runtime error if necessary. */
  def freeArray: (Label, List[Instruction]) = {
    (
      FreeArray.funcLabel,
      List[Instruction](
        Push(ListBuffer(LR)),
        Cmp(resultReg, ImmInt(FALSE_INT)),
        LdrCond(EQ, resultReg, DataLabel(Label(FreeArray.msgName(0)))),
        BranchCond(EQ, RuntimeError.funcLabel),
        BranchLink(Label("free")),
        Pop(ListBuffer(PC))
      )
    )
  }

  /* Checks if attempting to defererence a null pointer, branching to a
     runtime error if necessary. */
  def checkNullPointer: (Label, List[Instruction]) = {
    (
      NullPointer.funcLabel,
      List[Instruction](
        Push(ListBuffer(LR)),
        Cmp(resultReg, ImmInt(FALSE_INT)),
        LdrCond(EQ, resultReg, DataLabel(Label(NullPointer.msgName(0)))),
        BranchLinkCond(EQ, RuntimeError.funcLabel),
        Pop(ListBuffer(PC))
      )
    )
  }

  def negativeShift: (Label, List[Instruction]) = {
    (
      NegativeShift.funcLabel,
      List[Instruction](
        Push(ListBuffer(LR)),
        Cmp(resultReg, ImmInt(FALSE_INT)),
        LdrCond(LT, resultReg, DataLabel(Label(ArrayBounds.msgName(0)))),
        BranchLinkCond(LT, RuntimeError.funcLabel)
      )
    )
  }

}
