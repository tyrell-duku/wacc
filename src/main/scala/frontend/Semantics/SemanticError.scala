import Rules._

sealed trait SemanticError {
  override def toString: String = this match {
    case typeMismatch(invalid, actualT, expected) =>
      "Type mismatch at " + invalid + " expected type " + expected.mkString(
        " or "
      ) + " but actual type " + actualT
    case arrayOutOfBounds(id) => "Array index out of bounds for array " + id
    case elementAccessDenied(id) =>
      "Element access of " + id + " is not permitted"
    case invalidReturn(e) => "Invalid return statement from main: return " + e
    case functionIllegalAssignment(id) =>
      "Illegal assignment to function " + id.s
    case invalidParams(id, actual, expected) =>
      "Invalid params for function " + id.s + " Expected number of params: " + expected + ". Actual: " + actual
    case invalidPairElem(pe) =>
      "Invalid input, expected: Pair, unable to perform " + pe
    case functionDeclared(id)    => identDeclared(id).toString()
    case functionNotDeclared(id) => identNotDeclared("Function", id).toString
    case variableNotDeclared(id) => identNotDeclared("Variable", id).toString()
    case variableDeclared(id)    => identDeclared(id).toString()
    case identNotDeclared(t, id) =>
      t + " " + id + " not declared in current scope"
    case identDeclared(id) => "Conflicting definitions for variable " + id
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
case class functionDeclared(id: Ident) extends SemanticError
case class functionNotDeclared(id: Ident) extends SemanticError
case class invalidParams(id: Ident, actual: Int, expected: Int)
    extends SemanticError
case class invalidPairElem(pe: PairElem) extends SemanticError
case class identDeclared(id: Ident) extends SemanticError
case class identNotDeclared(t: String, id: Ident) extends SemanticError
