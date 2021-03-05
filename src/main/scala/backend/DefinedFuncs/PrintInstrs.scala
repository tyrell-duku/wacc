package backend.DefinedFuncs

import backend.CodeGeneration.Expressions._
import backend.CodeGenerator._
import backend.IR.Condition.{EQ, NE}
import backend.IR.InstructionSet._
import backend.IR.Operand._
import PreDefinedFuncs._

import scala.collection.mutable.ListBuffer

object PrintInstrs {
  case object PrintInt extends PreDefFunc {
    override val funcLabel = Label("p_print_int")
    override val msgName = List("msg_int")
    override val functionMsg = List("%d\\0")
    override val func = intPrintInstrs
  }
  case object PrintBool extends PreDefFunc {
    override val funcLabel = Label("p_print_bool")
    override val msgName = List("msg_true", "msg_false")
    override val functionMsg = List("true\\0", "false\\0")
    override val func = boolPrintInstrs
  }
  case object PrintString extends PreDefFunc {
    override val funcLabel = Label("p_print_string")
    override val msgName = List("msg_string")
    override val functionMsg = List("%.*s\\0")
    override val func = stringPrintInstrs
  }
  case object PrintReference extends PreDefFunc {
    override val funcLabel = Label("p_print_reference")
    override val msgName = List("msg_reference")
    override val functionMsg = List("%p\\0")
    override val func = referencePrintInstrs
  }
  case object PrintLn extends PreDefFunc {
    override val funcLabel = Label("p_print_ln")
    override val msgName = List("msg_new_line")
    override val functionMsg = List("\\0")
    override val func = newLinePrintInstrs
  }

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
      LdrCond(NE, resultReg, DataLabel(Label("msg_true"))),
      LdrCond(EQ, resultReg, DataLabel(Label("msg_false"))),
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
