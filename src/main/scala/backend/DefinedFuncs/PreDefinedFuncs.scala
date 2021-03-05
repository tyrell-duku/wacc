package backend.DefinedFuncs

import backend.CodeGenerator.{dataTable, funcTable, resultReg}
import backend.IR.InstructionSet._
import backend.IR.Operand._
import backend.DefinedFuncs.PrintInstrs._
import backend.DefinedFuncs.RuntimeErrors._
import scala.collection.mutable.ListBuffer
import backend.IR.Condition._

object PreDefinedFuncs {
  sealed trait PreDefFunc {
    val funcLabel: Label
    val msgName: List[String]
    val msgs: List[String]
    val func: (Label, List[Instruction])
  }
  /* Runtime errors */
  // Checks for reference to array element at negative index or index that
  // exceeds the size of the array
  case object ArrayBounds extends PreDefFunc {
    override val funcLabel = Label("p_check_array_bounds")
    override val msgName = List("msg_neg_index", "msg_index_too_large")
    override val msgs = List(
      "ArrayIndexOutOfBoundsError: negative index\\n\\0",
      "ArrayIndexOutOfBoundsError: index too large\\n\\0"
    )
    override val func = checkArrayBounds
  }
  // Check for division by 0
  case object DivideByZero extends PreDefFunc {
    override val funcLabel = Label("p_check_divide_by_zero")
    override val msgName = List("msg_divide_by_zero")
    override val msgs =
      List("DivideByZeroError: divide or modulo by zero\\n\\0")
    override val func = checkDivideByZero
  }
  // Check for integer overflow/underflow
  case object Overflow extends PreDefFunc {
    override val funcLabel = Label("p_throw_overflow_error")
    override val msgName = List("msg_overflow")
    override val msgs =
      List(
        "OverflowError: the result is too small/large to store in a 4-byte signed-integer.\\n"
      )
    override val func = throwOverflowError
  }
  // Check for attempt to call 'free' on pair
  case object FreePair extends PreDefFunc {
    override val funcLabel = Label("p_free_pair")
    override val msgName = List("msg_null_reference")
    override val msgs =
      List("NullReferenceError: dereference a null reference\\n\\0")
    override val func = freePair
  }
  // Check for attempt to free an array
  case object FreeArray extends PreDefFunc {
    override val funcLabel = Label("p_free_array")
    override val msgName = List("msg_null_reference")
    override val msgs =
      List("NullReferenceError: dereference a null reference\\n\\0")
    override val func = freeArray
  }
  // Dereferencing a null pointer
  case object NullPointer extends PreDefFunc {
    override val funcLabel = Label("p_check_null_pointer")
    override val msgName = List("msg_null_reference")
    override val msgs =
      List("NullReferenceError: dereference a null reference\\n\\0")
    override val func = checkNullPointer
  }
  // Throws a runtime error
  case object RuntimeError extends PreDefFunc {
    override val funcLabel = Label("p_throw_runtime_error")
    override val msgName = List.empty[String]
    override val msgs = List.empty[String]
    override val func = throwRuntimeError
  }

  /* Printing */
  case object PrintInt extends PreDefFunc {
    override val funcLabel = Label("p_print_int")
    override val msgName = List("msg_int")
    override val msgs = List("%d\\0")
    override val func = intPrintInstrs
  }
  // Printing booleans
  case object PrintBool extends PreDefFunc {
    override val funcLabel = Label("p_print_bool")
    override val msgName = List("msg_true", "msg_false")
    override val msgs = List("true\\0", "false\\0")
    override val func = boolPrintInstrs
  }
  // Printing strings
  case object PrintString extends PreDefFunc {
    override val funcLabel = Label("p_print_string")
    override val msgName = List("msg_string")
    override val msgs = List("%.*s\\0")
    override val func = stringPrintInstrs
  }
  // Printing references to pairs/arrays
  case object PrintReference extends PreDefFunc {
    override val funcLabel = Label("p_print_reference")
    override val msgName = List("msg_reference")
    override val msgs = List("%p\\0")
    override val func = referencePrintInstrs
  }
  // Prints a new line
  case object PrintLn extends PreDefFunc {
    override val funcLabel = Label("p_print_ln")
    override val msgName = List("msg_new_line")
    override val msgs = List("\\0")
    override val func = newLinePrintInstrs
  }

  /* Reading */
  // Reads an int
  case object ReadInt extends PreDefFunc {
    override val funcLabel = Label("p_read_int")
    // function requires Label argument
    override val func = null
    override val msgs = List("%d\\0")
    override val msgName = List.empty
  }
  // Reads a character
  case object ReadChar extends PreDefFunc {
    override val funcLabel = Label("p_read_char")
    // function requires Label argument
    override val func = null
    override val msgs = List(" %c\\0")
    override val msgName = List.empty
  }

}
