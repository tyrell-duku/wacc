package backend

import InstructionSet._
import frontend.Rules._
import scala.collection.mutable.ListBuffer

object ReadInstructions {

  final val resultReg: Reg = R0

  def charRead(label: Label): (Label, List[Instruction]) = {
    (
      Label("p_read_char"),
      List[Instruction](
        Push(ListBuffer(LR)),
        Mov(R1, resultReg),
        Ldr(resultReg, DataLabel(label)),
        Add(resultReg, resultReg, ImmInt(4)),
        BranchLink(Label("scanf")),
        Pop(ListBuffer(PC))
      )
    )
  }

  def intRead(label: Label): (Label, List[Instruction]) = {
    (
      Label("p_read_int"),
      List[Instruction](
        Push(ListBuffer(LR)),
        Mov(R1, resultReg),
        Ldr(resultReg, DataLabel(label)),
        Add(resultReg, resultReg, ImmInt(4)),
        BranchLink(Label("scanf")),
        Pop(ListBuffer(PC))
      )
    )
  }
}
