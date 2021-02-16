import java.io.File
import parsley.Failure
import parsley.Success
import scala.collection.mutable.ListBuffer

object Main {
  val syntaxError = 100
  val semanticError = 200

  def syntaxExit(errorMessage: String) = {
    println("Syntax Error")
    println(errorMessage)
    sys.exit(syntaxError)
  }

  def semanticExit(errorMessages: ListBuffer[SemanticError]) = {
    println("Semantic Error")
    errorMessages.foreach((semErr: SemanticError) => println(semErr))
    sys.exit(semanticError)
  }

  def semanticAnalysis(result: Rules.Program) = {
    val semanticChecker = new SemanticChecker
    val semanticResult = semanticChecker.progAnalysis(result)
    if (semanticResult.nonEmpty) {
      semanticExit(semanticResult)
    }
  }

  def main(args: Array[String]) = {
    if (args.length == 0) {
      throw new Exception("Invalid number of arguments, please enter a file.")
    }

    val file = new File(args(0))
    if (!file.exists()) {
      throw new Exception("File not present, please enter a valid file.")
    }

    val parserResult = Parser.waccParser.parseFromFile(file)

    parserResult match {
      case Failure(msg) => syntaxExit(msg)
      case Success(x)   => semanticAnalysis(x)
    }
  }
}
