package backend

import scala.collection.mutable.ListBuffer
import InstructionSet.Data
import InstructionSet.Label
import frontend.Rules.StrLiter

class DataTable {
  var table = ListBuffer.empty[Data]
  val stringDataSkeleton = "msg_"
  var dataCount = 0

  def addDataEntry(string: StrLiter): Label = {
    addDataEntry(string.toString())
  }

  def addDataEntry(string: String): Label = {
    val msgLabel = Label(getNextLabel())
    table += Data(msgLabel, string)
    msgLabel
  }

  def addDataEntryWithLabel(label: String, string: String): Label = {
    val msgLabel = Label(label)
    if (!containsLabel(msgLabel)) {
      table += Data(msgLabel, string)
      return msgLabel
    }
    null
  }

  private def getNextLabel(): String = {
    val nextLabel = stringDataSkeleton + dataCount.toString()
    dataCount += 1
    nextLabel
  }

  private def containsLabel(label: Label): Boolean = {
    for (d <- table) {
      val Data(l, _) = d
      if (l == label) {
        return true
      }
    }
    false
  }

}
