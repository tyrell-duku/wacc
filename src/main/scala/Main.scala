import java.io.File

import frontend.Rules.Program
import frontend.Semantics._
import frontend._
import frontend.SSA
import parsley.Failure
import parsley.Success

import scala.collection.mutable.ListBuffer
import backend.{ARMPrinter, CodeGenerator, Peephole}

object Main {
  // Constants for error codes
  private val syntaxError = 100
  private val semanticError = 200

  /* Exit with code 100 & print error msg from Parser */
  private def syntaxExit(errorMessage: String): Nothing = {
    println("Syntax Error")
    println(errorMessage)
    sys.exit(syntaxError)
  }

  /* Exit with code 200 & print all semantic errors returned by
     SemanticChecker. */
  private def semanticExit(
      errorMessages: ListBuffer[SemanticError]
  ): Nothing = {
    println("Semantic Error")
    errorMessages.foreach((semErr: SemanticError) => println(semErr))
    sys.exit(semanticError)
  }

  /* Perform semanticAnalysis on AST returned by Parser. */
  private def semanticAnalysis(result: Program): SymbolTable = {
    val semanticChecker = new SemanticChecker
    val (sTable, semanticResult) = semanticChecker.progAnalysis(result)
    // If nonEmpty, semantic errors occur so semantic exit
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
      case Success(ast) =>
        val sTable = semanticAnalysis(ast)
        val ssa = SSA(sTable)
        val (prunedAst, stackSize) = ssa.toSSA(ast)
        val (data, instrs) =
          CodeGenerator.transProg(prunedAst, sTable, stackSize)
        ARMPrinter.execute(
          file.getName(),
          data,
          Peephole.optimiseBlocks(instrs)
        )
    }
  }
}
