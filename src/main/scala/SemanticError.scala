import Rules._

sealed trait SemanticError {
  override def toString: String = this match {
    case typeMismatch(invalid, actualT, expected) =>
      "Type mismatch at " + invalid + "LHS is of type " + expected + "but RHS of type " + actualT
    case variableNotDeclared(id) =>
      "Variable" + id + "not declared in current scope"
    case variableDeclared(id) => "Conflicting definitions for variable " + id
    case arrayOutOfBounds(id) => "Array index out of bounds for array " + id
    case elementAccessDenied(id) =>
      "Element access of " + id + " is not permitted"
    case invalidReturn(e) => "Invalid return statement from main: return " + e
  }
}

case class typeMismatch(invalid: AssignRHS, actualT: Type, expected: Type)
    extends SemanticError
case class variableNotDeclared(id: Ident) extends SemanticError
case class variableDeclared(id: Ident) extends SemanticError
case class arrayOutOfBounds(id: Ident) extends SemanticError
case class elementAccessDenied(id: Ident) extends SemanticError
case class invalidReturn(e: Expr) extends SemanticError
