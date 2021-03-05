package backend.DataTypes

import backend.IR.InstructionSet.{Instruction, Label}

import scala.collection.mutable.ListBuffer

class FuncTable {
  /* For unique label allocation */
  private var labelCounter = 0
  val table = ListBuffer.empty[(Label, List[Instruction])]

  /* Add user defined assembly function to table. */
  def addEntry(label: Label, instrs: ListBuffer[Instruction]): Unit = {
    table += ((label, instrs.toList))
  }

  /* Add pre-defined assembly function to table if not currently present.
     Contains check avoids potential duplication of predefined functions in
     table. */
  def addEntry(tup: (Label, List[Instruction])): Unit = {
    val (label, instrs) = tup
    if (!contains(label)) {
      val entry = (label, instrs)
      table += entry
    }
  }

  /* Check if a function label is present in the table */
  private def contains(label: Label): Boolean = {
    for ((l, _) <- table) {
      if (label == l) {
        return true
      }
    }
    false
  }

  /* Return a unique label for branched off assembly functions */
  def assignLabel(): Label = {
    val nextLabel = Label("L" + labelCounter)
    labelCounter += 1
    nextLabel
  }
}
