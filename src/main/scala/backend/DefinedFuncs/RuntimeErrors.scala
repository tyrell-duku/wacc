package backend.DefinedFuncs

import backend.CodeGenerator.{dataTable, funcTable, resultReg}
import backend.DefinedFuncs.PreDefinedFuncs._
import backend.DefinedFuncs.PrintInstrs.stringPrintInstrs
import backend.IR.InstructionSet._
import backend.IR.Operand._
import scala.collection.mutable.ListBuffer
import backend.IR.Condition._

object RuntimeErrors {
  def addRuntimeError(err: PreDefFunc): Label = {
    funcTable.addEntry(RuntimeError.func)
    funcTable.addEntry(stringPrintInstrs)
    dataTable.addDataEntryWithLabel("msg_string", "%.*s\\0")
    for (i <- 0 until err.functionMsg.length) {
      dataTable.addDataEntryWithLabel(err.msgName(i), err.functionMsg(i))
    }
    funcTable.addEntry(err.func)
    err.funcLabel
  }

  def throwRuntimeError: (Label, List[Instruction]) = {
    (
      RuntimeError.funcLabel,
      List[Instruction](
        BranchLink(Label("p_print_string")),
        Mov(resultReg, ImmInt(-1)),
        BranchLink(Label("exit"))
      )
    )
  }

  def checkArrayBounds: (Label, List[Instruction]) = {
    (
      ArrayBounds.funcLabel,
      List[Instruction](
        Push(ListBuffer(LR)),
        Cmp(resultReg, ImmInt(0)),
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

  def checkDivideByZero: (Label, List[Instruction]) = {
    (
      DivideByZero.funcLabel,
      List[Instruction](
        Push(ListBuffer(LR)),
        Cmp(R1, ImmInt(0)),
        LdrCond(EQ, resultReg, DataLabel(Label(DivideByZero.msgName(0)))),
        BranchLinkCond(EQ, RuntimeError.funcLabel),
        Pop(ListBuffer(PC))
      )
    )
  }

  def throwOverflowError: (Label, List[Instruction]) = {
    (
      Overflow.funcLabel,
      List[Instruction](
        Ldr(resultReg, DataLabel(Label(Overflow.msgName(0)))),
        BranchLink(RuntimeError.funcLabel)
      )
    )
  }

  def freePair: (Label, List[Instruction]) = {
    (
      FreePair.funcLabel,
      List[Instruction](
        Push(ListBuffer(LR)),
        Cmp(resultReg, ImmInt(0)),
        LdrCond(EQ, resultReg, DataLabel(Label(FreePair.msgName(0)))),
        BranchCond(EQ, RuntimeError.funcLabel),
        Push(ListBuffer(resultReg)),
        Ldr(resultReg, RegAdd(resultReg)),
        BranchLink(Label("free")),
        Ldr(resultReg, RegAdd(SP)),
        Ldr(resultReg, RegisterOffset(resultReg, 4)),
        BranchLink(Label("free")),
        Pop(ListBuffer(resultReg)),
        BranchLink(Label("free")),
        Pop(ListBuffer(PC))
      )
    )
  }

  def freeArray: (Label, List[Instruction]) = {
    (
      FreeArray.funcLabel,
      List[Instruction](
        Push(ListBuffer(LR)),
        Cmp(resultReg, ImmInt(0)),
        LdrCond(EQ, resultReg, DataLabel(Label(FreeArray.msgName(0)))),
        BranchCond(EQ, RuntimeError.funcLabel),
        BranchLink(Label("free")),
        Pop(ListBuffer(PC))
      )
    )
  }

  def checkNullPointer: (Label, List[Instruction]) = {
    (
      NullPointer.funcLabel,
      List[Instruction](
        Push(ListBuffer(LR)),
        Cmp(resultReg, ImmInt(0)),
        LdrCond(EQ, resultReg, DataLabel(Label(NullPointer.msgName(0)))),
        BranchLinkCond(EQ, RuntimeError.funcLabel),
        Pop(ListBuffer(PC))
      )
    )
  }
}
