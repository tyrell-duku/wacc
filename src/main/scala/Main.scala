import java.io.File

import frontend.Rules.Program
import frontend.Parser.waccParser
import parsley.{Success, Failure}
import frontend.Semantics.{SymbolTable, SemanticChecker}
import frontend.SSA
import backend.CodeGenerator.transProg
import backend.Peephole.optimiseBlocks
import backend.ARMPrinter.printARM
import scala.collection.mutable

object Main {
  // Constants for error codes
  private val Syntax_Error = 100
  private val Semantic_Error = 200
  // General constants
  private val First_Arg = 0

  /* Exit with code 100 & print error msg from Parser */
  private def syntaxExit(errorMessage: String): Nothing = {
    println("Syntax Error")
    println(errorMessage)
    sys.exit(Syntax_Error)
  }

  /* Exit with code 200 & print all semantic errors returned by
     SemanticChecker and Runtime errors detected at compile time. */
  private def semanticExit(errorMessages: mutable.ListBuffer[_]): Nothing = {
    println("Semantic Error")
    errorMessages.foreach(semErr => println(semErr))
    sys.exit(Semantic_Error)
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

    val file = new File(args(First_Arg))
    if (!file.exists()) {
      throw new Exception("File not present, please enter a valid file.")
    }

    val parserResult = waccParser.parseFromFile(file)
    parserResult match {
      case Failure(msg) => syntaxExit(msg)
      case Success(ast) =>
        val sTable = semanticAnalysis(ast)
        val ssa = SSA(sTable)
        val (prunedAst, runtimeErrs, stackSize) = ssa.toSSA(ast)
        if (runtimeErrs.nonEmpty) {
          semanticExit(runtimeErrs)
        }
        val (data, instrs) = transProg(prunedAst, sTable, stackSize)
        printARM(file.getName(), data, optimiseBlocks(instrs))
    }
  }
}
