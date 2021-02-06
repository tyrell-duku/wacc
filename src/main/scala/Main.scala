object Main {
  def main(args: Array[String]) = {
    println("Hello, World")
    Parser.run()
  }

  def succ(x: Int): Int = {
    x + 1
  }
}
