import Rules._

sealed trait SemanticError {
  override def toString: String = this match {
    case typeMismatch(invalid, actualT, expected) =>
      "Type mismatch at " + invalid + " expected type " + expected.mkString(
        " or "
      ) + " but actual type " + actualT
    case variableNotDeclared(id) =>
      "Variable " + id + " not declared in current scope"
    case variableDeclared(id) => "Conflicting definitions for variable " + id
    case arrayOutOfBounds(id) => "Array index out of bounds for array " + id
    case elementAccessDenied(id) =>
      "Element access of " + id + " is not permitted"
    case invalidReturn(e) => "Invalid return statement from main: return " + e
    case functionIllegalAssignment(id) =>
      "Illegal assignment to function " + id.s
    case functionNotDeclared(id) => "Function " + id.s + " not declared"
    case invalidParams(id, actual, expected) =>
      "Invalid params for function " + id.s + " Expected number of params: " + expected + ". Actual: " + actual
    case invalidPairElem(pe) =>
      "Invalid input, expected: Pair, unable to perform " + pe
  }
}

case class typeMismatch(invalid: Any, actualT: Type, expected: List[Type])
    extends SemanticError
case class variableNotDeclared(id: Ident) extends SemanticError
case class variableDeclared(id: Ident) extends SemanticError
case class arrayOutOfBounds(id: Ident) extends SemanticError
case class elementAccessDenied(id: Ident) extends SemanticError
case class invalidReturn(e: Expr) extends SemanticError
case class functionIllegalAssignment(id: Ident) extends SemanticError
case class functionNotDeclared(id: Ident) extends SemanticError
case class invalidParams(id: Ident, actual: Int, expected: Int)
    extends SemanticError
case class invalidPairElem(pe: PairElem) extends SemanticError
