package backend.DataTypes

import backend.IR.InstructionSet.{Instruction, Label}

import scala.collection.mutable.ListBuffer

class FuncTable {
  private var labelCounter = 0
  var table = ListBuffer.empty[(Label, List[Instruction])]

  def addEntry(label: Label, instrs: List[Instruction]): Unit = {
    if (!contains(label)) {
      val entry = (label, instrs)
      table += entry
    }
  }

  def addEntry(tup: (Label, List[Instruction])): Unit = {
    val (label, instrs) = tup
    if (!contains(label)) {
      val entry = (label, instrs)
      table += entry
    }
  }

  private def contains(label: Label): Boolean = {
    for ((l, _) <- table) {
      if (label == l) {
        return true
      }
    }
    false
  }

  def assignLabel(): Label = {
    val nextLabel = Label("L" + labelCounter)
    labelCounter += 1
    nextLabel
  }
}
