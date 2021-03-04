package backend

import InstructionSet._
import frontend.Rules._
import scala.collection.mutable.ListBuffer

sealed trait RuntimeError
case object ArrayBounds extends RuntimeError
case object DivideByZero extends RuntimeError
case object Overflow extends RuntimeError
case object FreePair extends RuntimeError
case object NullPointer extends RuntimeError

object RuntimeErrors {

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
