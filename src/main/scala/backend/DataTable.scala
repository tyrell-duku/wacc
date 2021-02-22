package backend

case class MetaData(string: String)

class DataTable {
  var map = Map.empty[String, MetaData]
  val stringDataSkeleton = "msg_"
  var dataCount = 0

  def addDataEntry(string: List[Rules.Character]) = {
    map += (getNextLabel() -> MetaData(string.toString()))
  }

  private def getNextLabel(): String = {
    dataCount += 1
    stringDataSkeleton + dataCount.toString()
  }

  def getCurrLabel(): String = {
    stringDataSkeleton + dataCount.toString()
  }

  override def toString() = {
    map.toString()
  }

}
