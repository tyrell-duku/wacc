package backend.DataTypes

import backend.IR.InstructionSet.{Data, Label}
import frontend.Rules.StrLiter

import scala.collection.mutable.ListBuffer

class DataTable {
  var table = ListBuffer.empty[Data]
  val stringDataSkeleton = "msg_"
  var dataCount = 0
  /* Adds a string-literal STRING to the data table. */
  def addDataEntry(string: StrLiter): Label = {
    addDataEntry(string.toString())
  }
  /* Adds a string STRING to the data table. */
  def addDataEntry(string: String): Label = {
    val msgLabel = Label(getNextLabel())
    table += Data(msgLabel, string)
    msgLabel
  }
  /* Creates a data entry with label LABEL and string STRING and adds the
  entry to the data table. */
  def addDataEntryWithLabel(label: String, string: String): Label = {
    val msgLabel = Label(label)
    if (!containsLabel(msgLabel)) {
      table += Data(msgLabel, string)
      return msgLabel
    }
    null
  }
  /* Returns the name of the next label (of the form "msg_[label-count]"). */
  private def getNextLabel(): String = {
    val nextLabel = stringDataSkeleton + dataCount.toString
    dataCount += 1
    nextLabel
  }
  /* Returns true if the data table contains label LABEL and false otherwise. */
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
