package backend.DefinedFuncs

import backend.CodeGeneration.Expressions._
import backend.CodeGenerator._
import backend.IR.Condition.{EQ, NE}
import backend.IR.InstructionSet._
import backend.IR.Operand._
import PreDefinedFuncs._

import scala.collection.mutable.ListBuffer

object PrintInstrs {
  def stringPrintInstrs: (Label, List[Instruction]) = (
    PrintString.funcLabel,
    List[Instruction](
      Push(ListBuffer(LR)),
      Ldr(R1, RegAdd(resultReg)),
      Add(R2, resultReg, ImmInt(4)),
      Ldr(resultReg, DataLabel(Label(PrintString.msgName(0)))),
      Add(resultReg, resultReg, ImmInt(4)),
      BranchLink(Label("printf")),
      Mov(resultReg, ImmInt(0)),
      BranchLink(Label("fflush")),
      Pop(ListBuffer(PC))
    )
  )

  def boolPrintInstrs: (Label, List[Instruction]) = (
    PrintBool.funcLabel,
    List[Instruction](
      Push(ListBuffer(LR)),
      Cmp(resultReg, ImmInt(0)),
      LdrCond(NE, resultReg, DataLabel(Label(PrintBool.msgName(0)))),
      LdrCond(EQ, resultReg, DataLabel(Label(PrintBool.msgName(1)))),
      Add(resultReg, resultReg, ImmInt(4)),
      BranchLink(Label("printf")),
      Mov(resultReg, ImmInt(0)),
      BranchLink(Label("fflush")),
      Pop(ListBuffer(PC))
    )
  )

  def intPrintInstrs: (Label, List[Instruction]) = (
    PrintInt.funcLabel,
    List[Instruction](
      Push(ListBuffer(LR)),
      Mov(R1, resultReg),
      Ldr(resultReg, DataLabel(Label(PrintInt.msgName(0)))),
      Add(resultReg, resultReg, ImmInt(4)),
      BranchLink(Label("printf")),
      Mov(resultReg, ImmInt(0)),
      BranchLink(Label("fflush")),
      Pop(ListBuffer(PC))
    )
  )

  def referencePrintInstrs: (Label, List[Instruction]) =
    (
      PrintReference.funcLabel,
      List[Instruction](
        Push(ListBuffer(LR)),
        Mov(R1, resultReg),
        Ldr(resultReg, DataLabel(Label(PrintReference.msgName(0)))),
        Add(resultReg, resultReg, ImmInt(4)),
        BranchLink(Label("printf")),
        Mov(resultReg, ImmInt(0)),
        BranchLink(Label("fflush")),
        Pop(ListBuffer(PC))
      )
    )

  def newLinePrintInstrs: (Label, List[Instruction]) =
    (
      PrintLn.funcLabel,
      List[Instruction](
        Push(ListBuffer(LR)),
        Ldr(resultReg, DataLabel(Label(PrintLn.msgName(0)))),
        Add(resultReg, resultReg, ImmInt(4)),
        BranchLink(Label("puts")),
        Mov(resultReg, ImmInt(0)),
        BranchLink(Label("fflush")),
        Pop(ListBuffer(PC))
      )
    )
}
