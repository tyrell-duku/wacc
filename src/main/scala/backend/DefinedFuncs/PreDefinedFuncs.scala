package backend.DefinedFuncs

import backend.CodeGenerator.{dataTable, funcTable}
import backend.DefinedFuncs.PrintInstrs.stringPrintInstrs
import backend.IR.InstructionSet._
import backend.IR.Operand._

import scala.collection.mutable.ListBuffer

object PreDefinedFuncs {

  sealed trait PreDefFunc
  case object ArrayBounds extends PreDefFunc
  case object DivideByZero extends PreDefFunc
  case object Overflow extends PreDefFunc
  case object FreePair extends PreDefFunc
  case object FreeArray extends PreDefFunc
  case object NullPointer extends PreDefFunc

  def addRuntimeError(err: PreDefFunc): Label = {
    funcTable.addEntry(throwRuntimeError())
    funcTable.addEntry(stringPrintInstrs)
    dataTable.addDataEntryWithLabel("msg_string", "%.*s\\0")
    err match {
      case ArrayBounds =>
        funcTable.addEntry(
          checkArrayBounds(
            dataTable.addDataEntryWithLabel(
              "msg_neg_index",
              "ArrayIndexOutOfBoundsError: negative index\\n\\0"
            ),
            dataTable.addDataEntryWithLabel(
              "msg_index_too_large",
              "ArrayIndexOutOfBoundsError: index too large\\n\\0"
            )
          )
        )
        Label("p_check_array_bounds")
      case DivideByZero =>
        funcTable.addEntry(
          checkDivideByZero(
            dataTable.addDataEntryWithLabel(
              "msg_divide_by_zero",
              "DivideByZeroError: divide or modulo by zero\\n\\0"
            )
          )
        )
        Label("p_check_divide_by_zero")
      case Overflow =>
        funcTable.addEntry(
          throwOverflowError(
            dataTable.addDataEntryWithLabel(
              "msg_overflow",
              "OverflowError: the result is too small/large to store in a 4-byte signed-integer.\\n"
            )
          )
        )
        Label("p_throw_overflow_error")
      case FreePair =>
        funcTable.addEntry(
          freePair(
            dataTable.addDataEntryWithLabel(
              "msg_null_reference",
              "NullReferenceError: dereference a null reference\\n\\0"
            )
          )
        )
        Label("p_free_pair")
      case FreeArray =>
        funcTable.addEntry(
          freeArray(
            dataTable.addDataEntry(
              "NullReferenceError: dereference a null reference\\n\\0"
            )
          )
        )
        Label("p_free_array")
      case NullPointer =>
        funcTable.addEntry(
          checkNullPointer(
            dataTable.addDataEntryWithLabel(
              "msg_null_reference",
              "NullReferenceError: dereference a null reference\\n\\0"
            )
          )
        )
        Label("p_check_null_pointer")
    }
  }

  def throwRuntimeError(): (Label, List[Instruction]) = {
    (
      Label("p_throw_runtime_error"),
      List[Instruction](
        BranchLink(Label("p_print_string")),
        Mov(R0, ImmInt(-1)),
        BranchLink(Label("exit"))
      )
    )
  }

  def checkArrayBounds(
      label1: Label,
      label2: Label
  ): (Label, List[Instruction]) = {
    (
      Label("p_check_array_bounds"),
      List[Instruction](
        Push(ListBuffer(LR)),
        Cmp(R0, ImmInt(0)),
        LdrLT(R0, DataLabel(label1)),
        BranchLinkLT(Label("p_throw_runtime_error")),
        Ldr(R1, RegAdd(R1)),
        Cmp(R0, R1),
        LdrCS(R0, DataLabel(label2)),
        BranchLinkCS(Label("p_throw_runtime_error")),
        Pop(ListBuffer(PC))
      )
    )
  }

  def checkDivideByZero(label: Label): (Label, List[Instruction]) = {
    (
      Label("p_check_divide_by_zero"),
      List[Instruction](
        Push(ListBuffer(LR)),
        Cmp(R1, ImmInt(0)),
        LdrEQ(R0, DataLabel(label)),
        BranchLinkEQ(Label("p_throw_runtime_error")),
        Pop(ListBuffer(PC))
      )
    )
  }

  def throwOverflowError(label: Label): (Label, List[Instruction]) = {
    (
      Label("p_throw_overflow_error"),
      List[Instruction](
        Ldr(R0, DataLabel(label)),
        BranchLink(Label("p_throw_runtime_error"))
      )
    )
  }

  def freePair(label: Label): (Label, List[Instruction]) = {
    (
      Label("p_free_pair"),
      List[Instruction](
        Push(ListBuffer(LR)),
        Cmp(R0, ImmInt(0)),
        LdrEQ(R0, DataLabel(label)),
        BranchEq(Label("p_throw_runtime_error")),
        Push(ListBuffer(R0)),
        Ldr(R0, RegAdd(R0)),
        BranchLink(Label("free")),
        Ldr(R0, RegAdd(SP)),
        Ldr(R0, RegisterOffset(R0, 4)),
        BranchLink(Label("free")),
        Pop(ListBuffer(R0)),
        BranchLink(Label("free")),
        Pop(ListBuffer(PC))
      )
    )
  }

  def freeArray(label: Label): (Label, List[Instruction]) = {
    (
      Label("p_free_pair"),
      List[Instruction](
        Push(ListBuffer(LR)),
        Cmp(R0, ImmInt(0)),
        LdrEQ(R0, DataLabel(label)),
        BranchEq(Label("p_throw_runtime_error")),
        BranchLink(Label("free")),
        Pop(ListBuffer(PC))
      )
    )
  }

  def checkNullPointer(label: Label): (Label, List[Instruction]) = {
    (
      Label("p_check_null_pointer"),
      List[Instruction](
        Push(ListBuffer(LR)),
        Cmp(R0, ImmInt(0)),
        LdrEQ(R0, DataLabel(label)),
        BranchLinkEQ(Label("p_throw_runtime_error")),
        Pop(ListBuffer(PC))
      )
    )
  }
}
