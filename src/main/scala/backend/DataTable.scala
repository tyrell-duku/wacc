package backend

import scala.collection.mutable.ListBuffer
import InstructionSet.Data
import InstructionSet.Label
import frontend.Rules.Character

class DataTable {
  var table = ListBuffer.empty[Data]
  val stringDataSkeleton = "msg_"
  var dataCount = 0

  def addDataEntry(string: List[Character]): Label = {
    val msgLabel = Label(getNextLabel())
    table += Data(msgLabel, string.toString())
    msgLabel
  }

  private def getNextLabel(): String = {
    val nextLabel = stringDataSkeleton + dataCount.toString()
    dataCount += 1
    nextLabel
  }

}
