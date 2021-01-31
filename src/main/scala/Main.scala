import scala.io.Source

object Main {
  def main(args: Array[String]) = {
    if (args.length == 0) {
      throw new Exception("Invalid arguements")
    }
    println(readFile(args(0)))
  }

  def readFile(str:String) : String = {
    return Source.fromFile(str).getLines.mkString("\n")
  }
}
