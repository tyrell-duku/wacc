import java.io.File

object Main {
  def main(args: Array[String]) = {
    if (args.length == 0) {
      throw new Exception("Invalid number of arguments, please enter a file.")
    }

    val file = new File(args(0))
    if (!file.exists()){
      throw new Exception("File not present, please enter a valid file.")
    }
  }
}
