import java.io.File

import frontend.Rules.Program
import frontend.Semantics._
import frontend._
import parsley.Failure
import parsley.Success

import scala.collection.mutable.ListBuffer
import backend.{ARMPrinter, CodeGenerator}

object Main {
  val syntaxError = 100
  val semanticError = 200

  def syntaxExit(errorMessage: String): Nothing = {
    println("Syntax Error")
    println(errorMessage)
    sys.exit(syntaxError)
  }

  def semanticExit(errorMessages: ListBuffer[SemanticError]): Nothing = {
    println("Semantic Error")
    errorMessages.foreach((semErr: SemanticError) => println(semErr))
    sys.exit(semanticError)
  }

  def semanticAnalysis(result: Program): SymbolTable = {
    val semanticChecker = new SemanticChecker
    val (sTable, semanticResult) = semanticChecker.progAnalysis(result)
    if (semanticResult.nonEmpty) {
      semanticExit(semanticResult)
    }
    sTable
  }

  def main(args: Array[String]): Unit = {
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
      case Success(x) =>
        val sTable = semanticAnalysis(x)
        val codeGen = new CodeGenerator(sTable)
        val (data, instrs) = codeGen.transProg(x)
        ARMPrinter.execute(file.getName(), data, instrs)
    }
  }
}
