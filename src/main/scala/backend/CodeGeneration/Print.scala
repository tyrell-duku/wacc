package backend.CodeGeneration

import backend.CodeGeneration.Expressions.transExp
import backend.CodeGenerator._
import backend.DefinedFuncs.PreDefinedFuncs._
import backend.IR.InstructionSet._
import backend.IR.Operand.R0
import frontend.Rules.{
  Type,
  CharT,
  IntT,
  BoolT,
  StringT,
  Pair,
  ArrayT,
  Expr,
  PrintLn => _
}

import scala.collection.mutable.ListBuffer
import backend.DefinedFuncs.PreDefinedFuncs

object Print {

  private def typeToPreDefFunc(t: Type): PreDefinedFuncs.PreDefFunc = {
    t match {
      case IntT          => PrintInt
      case BoolT         => PrintBool
      case StringT       => PrintString
      case _: Pair       => PrintReference
      case ArrayT(CharT) => PrintString
      case ArrayT(_)     => PrintReference
      // CharT does not have a pre defined function
      case _ => null
    }
  }

  def transPrint(
      e: Expr,
      isNewLine: Boolean
  ): ListBuffer[Instruction] = {
    val t = getExprType(e)
    val freeReg = getFreeReg()
    val instrs = transExp(e, freeReg)
    instrs += Mov(resultReg, freeReg)

    t match {
      case CharT => instrs += BranchLink(Label("putchar"))
      case _ =>
        val printFunc = typeToPreDefFunc(t)
        for (i <- 0 until printFunc.msgs.length) {
          dataTable.addDataEntryWithLabel(
            printFunc.msgName(i),
            printFunc.msgs(i)
          )
        }
        instrs += BranchLink(printFunc.funcLabel)
        funcTable.addEntry(printFunc.func)
    }

    if (isNewLine) {
      instrs += BranchLink(
        PrintLn.funcLabel
      )
      dataTable.addDataEntryWithLabel(
        PrintLn.msgName(0),
        PrintLn.msgs(0)
      )
      funcTable.addEntry(PrintLn.func)
    }
    addUnusedReg(freeReg)
    instrs
  }

}
