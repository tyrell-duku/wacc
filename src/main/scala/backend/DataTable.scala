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
    val msgLabel = Label(getNextLabel())
    table += Data(msgLabel, string.toString())
    msgLabel
  }

  def addDataEntryWithLabel(label: String, string: String): Label = {
    val msgLabel = Label(label)
    table += Data(msgLabel, string)
    msgLabel
  }

  private def getNextLabel(): String = {
    val nextLabel = stringDataSkeleton + dataCount.toString()
    dataCount += 1
    nextLabel
  }

}
