package backend

import scala.collection.mutable.ListBuffer
import InstructionSet.Data
import InstructionSet.Label
import frontend.Rules._

class DataTable {
  var table = ListBuffer.empty[Data]
  val stringDataSkeleton = "msg_"
  var dataCount = 0

  def addDataEntry(string: List[Character]) = {
    table += (Data(Label(getNextLabel()), string.toString()))
  }

  private def getNextLabel(): String = {
    dataCount += 1
    stringDataSkeleton + dataCount.toString()
  }

  def getCurrLabel(): String = {
    stringDataSkeleton + dataCount.toString()
  }

  override def toString() = {
    table.toString()
  }

}
