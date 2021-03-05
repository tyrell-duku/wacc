package backend.CodeGeneration

import backend.CodeGeneration.Expressions.transExp
import backend.CodeGenerator._
import backend.DefinedFuncs.PrintInstrs._
import backend.IR.InstructionSet._
import backend.IR.Operand.R0
import frontend.Rules._

import scala.collection.mutable.ListBuffer

object Print {

  def transPrint(
      e: Expr,
      isNewLine: Boolean
  ): ListBuffer[Instruction] = {
    val t = getExprType(e)
    val freeReg = getFreeReg()
    val instrs = transExp(e, freeReg)
    instrs += Mov(R0, freeReg)
    t match {
      case CharT =>
        instrs += BranchLink(Label("putchar"))
      case IntT =>
        dataTable.addDataEntryWithLabel("msg_int", "%d\\0")
        instrs += BranchLink(Label("p_print_int"))
        funcTable.addEntry(intPrintInstrs)
      case BoolT =>
        dataTable.addDataEntryWithLabel("msg_true", "true\\0")
        dataTable.addDataEntryWithLabel("msg_false", "false\\0")
        instrs += BranchLink(Label("p_print_bool"))
        funcTable.addEntry(boolPrintInstrs)
      case StringT =>
        dataTable.addDataEntryWithLabel("msg_string", "%.*s\\0")
        instrs += BranchLink(Label("p_print_string"))
        funcTable.addEntry(stringPrintInstrs)
      case Pair(_, _) =>
        dataTable.addDataEntryWithLabel("msg_reference", "%p\\0")
        instrs += BranchLink(Label("p_print_reference"))
        funcTable.addEntry(referencePrintInstrs)
      case ArrayT(CharT) =>
        dataTable.addDataEntryWithLabel("msg_string", "%.*s\\0")
        instrs += BranchLink(Label("p_print_string"))
        funcTable.addEntry(stringPrintInstrs)
      case ArrayT(_) =>
        dataTable.addDataEntryWithLabel("msg_reference", "%p\\0")
        instrs += BranchLink(Label("p_print_reference"))
        funcTable.addEntry(referencePrintInstrs)

      case _ =>
    }
    if (isNewLine) {
      instrs += BranchLink(Label("p_print_ln"))
      dataTable.addDataEntryWithLabel("msg_new_line", "\\0")
      funcTable.addEntry(newLinePrintInstrs)
    }
    addUnusedReg(freeReg)
    instrs
  }

}
