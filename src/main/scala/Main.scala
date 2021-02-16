import java.io.File

object Main {
  val syntaxError = 100
  val semanticError = 200

  def main(args: Array[String]) = {
    if (args.length == 0) {
      throw new Exception("Invalid number of arguments, please enter a file.")
    }

    val file = new File(args(0))
    if (!file.exists()) {
      throw new Exception("File not present, please enter a valid file.")
    }

    val parserResult = Parser.waccParser.parseFromFile(file)

    if (parserResult.isFailure) {
      println("Syntax Error")
      println(parserResult)
      sys.exit(syntaxError)
    }

    val semanticChecker = new SemanticChecker
    val semanticResult = semanticChecker.progAnalysis(parserResult.get)

    if (semanticResult.nonEmpty) {
      println("Semantic Error")
      semanticResult.foreach((semErr: SemanticError) => println(semErr))
      sys.exit(semanticError)
    }
  }
}
