package backend

import InstructionSet._
import frontend.Rules._
import scala.collection.mutable.ListBuffer

object PrintInstrs {

  final val resultReg: Reg = R0

  val stringPrintInstrs: (Label, List[Instruction]) = (
    Label("p_print_string"),
    List[Instruction](
      Push(ListBuffer(LR)),
      Ldr(R1, RegAdd(resultReg)),
      Add(R2, resultReg, ImmInt(4)),
      Ldr(resultReg, DataLabel(Label("msg_string"))),
      Add(resultReg, resultReg, ImmInt(4)),
      BranchLink(Label("printf")),
      Mov(resultReg, ImmInt(0)),
      BranchLink(Label("fflush")),
      Pop(ListBuffer(PC))
    )
  )

  val boolPrintInstrs: (Label, List[Instruction]) = (
    Label("p_print_bool"),
    List[Instruction](
      Push(ListBuffer(LR)),
      Cmp(resultReg, ImmInt(0)),
      LdrCond(backend.NE, resultReg, DataLabel(Label("msg_true"))),
      LdrCond(backend.EQ, resultReg, DataLabel(Label("msg_false"))),
      Add(resultReg, resultReg, ImmInt(4)),
      BranchLink(Label("printf")),
      Mov(resultReg, ImmInt(0)),
      BranchLink(Label("fflush")),
      Pop(ListBuffer(PC))
    )
  )

  val intPrintInstrs: (Label, List[Instruction]) = (
    Label("p_print_int"),
    List[Instruction](
      Push(ListBuffer(LR)),
      Mov(R1, resultReg),
      Ldr(resultReg, DataLabel(Label("msg_int"))),
      Add(resultReg, resultReg, ImmInt(4)),
      BranchLink(Label("printf")),
      Mov(resultReg, ImmInt(0)),
      BranchLink(Label("fflush")),
      Pop(ListBuffer(PC))
    )
  )

  val referencePrintInstrs: (Label, List[Instruction]) =
    (
      Label("p_print_reference"),
      List[Instruction](
        Push(ListBuffer(LR)),
        Mov(R1, resultReg),
        Ldr(resultReg, DataLabel(Label("msg_reference"))),
        Add(resultReg, resultReg, ImmInt(4)),
        BranchLink(Label("printf")),
        Mov(resultReg, ImmInt(0)),
        BranchLink(Label("fflush")),
        Pop(ListBuffer(PC))
      )
    )

  val newLinePrintInstrs: (Label, List[Instruction]) =
    (
      Label("p_print_ln"),
      List[Instruction](
        Push(ListBuffer(LR)),
        Ldr(resultReg, DataLabel(Label("msg_new_line"))),
        Add(resultReg, resultReg, ImmInt(4)),
        BranchLink(Label("puts")),
        Mov(resultReg, ImmInt(0)),
        BranchLink(Label("fflush")),
        Pop(ListBuffer(PC))
      )
    )
}
