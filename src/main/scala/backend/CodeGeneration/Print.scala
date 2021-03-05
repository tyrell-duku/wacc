package backend.CodeGeneration

import backend.CodeGeneration.Expressions.transExp
import backend.CodeGenerator._
import backend.DefinedFuncs.PrintInstrs._
import backend.IR.InstructionSet._
import backend.IR.Operand.R0
import frontend.Rules._

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
        for (i <- 0 until printFunc.functionMsg.length) {
          dataTable.addDataEntryWithLabel(
            printFunc.msgName(i),
            printFunc.functionMsg(i)
          )
        }
        instrs += BranchLink(printFunc.funcLabel)
        funcTable.addEntry(printFunc.func)
    }

    if (isNewLine) {
      instrs += BranchLink(
        backend.DefinedFuncs.PrintInstrs.PrintLn.funcLabel
      )
      dataTable.addDataEntryWithLabel(
        backend.DefinedFuncs.PrintInstrs.PrintLn.msgName(0),
        backend.DefinedFuncs.PrintInstrs.PrintLn.functionMsg(0)
      )
      funcTable.addEntry(newLinePrintInstrs)
    }
    addUnusedReg(freeReg)
    instrs
  }

}
