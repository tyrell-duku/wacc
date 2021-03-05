package backend.DefinedFuncs

import backend.CodeGenerator.{dataTable, funcTable, resultReg}
import backend.IR.InstructionSet._
import backend.IR.Operand._
import backend.DefinedFuncs.PrintInstrs._
import backend.DefinedFuncs.RuntimeErrors._
import scala.collection.mutable.ListBuffer
import backend.IR.Condition._

object PreDefinedFuncs {

  trait PreDefFunc {
    val funcLabel: Label
    val msgName: List[String]
    val functionMsg: List[String]
    val func: (Label, List[Instruction])
  }
  case object ArrayBounds extends PreDefFunc {
    override val funcLabel = Label("p_check_array_bounds")
    override val msgName = List("msg_neg_index", "msg_index_too_large")
    override val functionMsg = List(
      "ArrayIndexOutOfBoundsError: negative index\\n\\0",
      "ArrayIndexOutOfBoundsError: index too large\\n\\0"
    )
    override val func = checkArrayBounds
  }
  case object DivideByZero extends PreDefFunc {
    override val funcLabel = Label("p_check_divide_by_zero")
    override val msgName = List("msg_divide_by_zero")
    override val functionMsg =
      List("DivideByZeroError: divide or modulo by zero\\n\\0")
    override val func = checkDivideByZero
  }
  case object Overflow extends PreDefFunc {
    override val funcLabel = Label("p_throw_overflow_error")
    override val msgName = List("msg_overflow")
    override val functionMsg =
      List(
        "OverflowError: the result is too small/large to store in a 4-byte signed-integer.\\n"
      )
    override val func = throwOverflowError
  }
  case object FreePair extends PreDefFunc {
    override val funcLabel = Label("p_free_pair")
    override val msgName = List("msg_null_reference")
    override val functionMsg =
      List("NullReferenceError: dereference a null reference\\n\\0")
    override val func = freePair

  }
  case object FreeArray extends PreDefFunc {
    override val funcLabel = Label("p_free_array")
    override val msgName = List("msg_null_reference")
    override val functionMsg =
      List("NullReferenceError: dereference a null reference\\n\\0")
    override val func = freeArray
  }
  case object NullPointer extends PreDefFunc {
    override val funcLabel = Label("p_check_null_pointer")
    override val msgName = List("msg_null_reference")
    override val functionMsg =
      List("NullReferenceError: dereference a null reference\\n\\0")
    override val func = checkNullPointer
  }
  case object RuntimeError extends PreDefFunc {
    override val funcLabel = Label("p_throw_runtime_error")
    override val msgName = List.empty[String]
    override val functionMsg = List.empty[String]
    override val func = throwRuntimeError
  }

  /* Printing */
  case object PrintInt extends PreDefFunc {
    override val funcLabel = Label("p_print_int")
    override val msgName = List("msg_int")
    override val functionMsg = List("%d\\0")
    override val func = intPrintInstrs
  }
  case object PrintBool extends PreDefFunc {
    override val funcLabel = Label("p_print_bool")
    override val msgName = List("msg_true", "msg_false")
    override val functionMsg = List("true\\0", "false\\0")
    override val func = boolPrintInstrs
  }
  case object PrintString extends PreDefFunc {
    override val funcLabel = Label("p_print_string")
    override val msgName = List("msg_string")
    override val functionMsg = List("%.*s\\0")
    override val func = stringPrintInstrs
  }
  case object PrintReference extends PreDefFunc {
    override val funcLabel = Label("p_print_reference")
    override val msgName = List("msg_reference")
    override val functionMsg = List("%p\\0")
    override val func = referencePrintInstrs
  }
  case object PrintLn extends PreDefFunc {
    override val funcLabel = Label("p_print_ln")
    override val msgName = List("msg_new_line")
    override val functionMsg = List("\\0")
    override val func = newLinePrintInstrs
  }

}
