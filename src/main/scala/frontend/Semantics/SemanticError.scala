import Rules._

sealed trait SemanticError {
  override def toString: String = this match {
    case TypeMismatch(invalid, actualT, expected) =>
      "Type mismatch of " + invalid + " at " + printPos(
        invalid.pos
      ) + " expected type " + expected.mkString(
        " or "
      ) + " but actual type " + actualT
    case ElementAccessDenied(id) =>
      "Element access of " + id + " at " + printPos(
        id.pos
      ) + " is not permitted"
    case InvalidReturn(e) =>
      "Invalid return statement from main of return " + e + " at " + printPos(
        e.pos
      )
    case FunctionIllegalAssignment(id) =>
      "Illegal assignment to function " + id + " at " + printPos(id.pos)
    case InvalidParams(id, actual, expected) =>
      "Invalid number of params for function " + id + " at " + printPos(
        id.pos
      ) + " expected: " + expected + " actual: " + actual
    case InvalidPairElem(pe) =>
      "Invalid input, expected: Pair, unable to perform " + pe + " at " + printPos(
        pe.pos
      )
    case FunctionDeclared(id)    => IdentDeclared(id).toString()
    case FunctionNotDeclared(id) => IdentNotDeclared("Function", id).toString
    case VariableNotDeclared(id) => IdentNotDeclared("Variable", id).toString()
    case VariableDeclared(id)    => IdentDeclared(id).toString()
    case IdentNotDeclared(t, id) =>
      t + " " + id + " at " + printPos(
        id.pos
      ) + " not declared in current scope"
    case IdentDeclared(id) =>
      "Conflicting definitions for variable " + id + " at " + printPos(id.pos)
  }

  def printPos(pos: (Int, Int)): String = pos match {
    case (line: Int, col: Int) => "(line " + line + ", column " + col + ")"
  }
}

case class TypeMismatch(invalid: AssignRHS, actualT: Type, expected: List[Type])
    extends SemanticError
case class VariableNotDeclared(id: Ident) extends SemanticError
case class VariableDeclared(id: Ident) extends SemanticError
case class ElementAccessDenied(id: Ident) extends SemanticError
case class InvalidReturn(e: Expr) extends SemanticError
case class FunctionIllegalAssignment(id: Ident) extends SemanticError
case class FunctionDeclared(id: Ident) extends SemanticError
case class FunctionNotDeclared(id: Ident) extends SemanticError
case class InvalidParams(id: Ident, actual: Int, expected: Int)
    extends SemanticError
case class InvalidPairElem(pe: PairElem) extends SemanticError
case class IdentDeclared(id: Ident) extends SemanticError
case class IdentNotDeclared(t: String, id: Ident) extends SemanticError
