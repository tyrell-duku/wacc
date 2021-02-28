package backend

import scala.collection.mutable.ListBuffer
import InstructionSet.Label
import frontend.Rules.StrLiter
import InstructionSet._

class FuncTable {
  var table = ListBuffer.empty[(Label, List[Instruction])]

  def addEntry(label: Label, instrs: List[Instruction]) {
    if (!contains(label)) {
      val entry = (label, instrs)
      table += entry
    }
  }

  def addEntry(tup: (Label, List[Instruction])) {
    val (label, instrs) = tup
    if (!contains(label)) {
      val entry = (label, instrs)
      table += entry
    }
    println("Doesnt contains")
  }

  private def contains(label: Label): Boolean = {
    for ((l, _) <- table) {
      if (label == l) {
        return true
      }
    }
    false
  }
}
