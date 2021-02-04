import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._
import parsley.Success

class UnaryOpTest extends AnyFunSuite {
  test("Successful parses not operator") {
    assertResult(true) { unaryOp.runParser("!").isSuccess }
  }

  test("Successful parses negation operator") {
    assertResult(true) { unaryOp.runParser("-").isSuccess }
  }

  test("Successful parses len operator") {
    assertResult(true) { unaryOp.runParser("len").isSuccess }
  }

  test("Successful parses ord operator") {
    assertResult(true) { unaryOp.runParser("ord").isSuccess }
  }

  test("Successful parses chr operator") {
    assertResult(true) { unaryOp.runParser("chr").isSuccess }
  }

  test("Successfully fails to parse random string '##!!!'") {
    assertResult(true) { unaryOp.runParser("##!!!").isFailure }
  }

  test("Successfully fails to parse random string 'l3n'") {
    assertResult(true) { unaryOp.runParser("l3n").isFailure }
  }
}

class BinaryOpTest extends AnyFunSuite {
  test("Successful parses * operator") {
    assertResult(true) { binaryOp.runParser("*").isSuccess }
  }

  test("Successful parses / operator") {
    assertResult(true) { binaryOp.runParser("/").isSuccess }
  }

  test("Successful parses % operator") {
    assertResult(true) { binaryOp.runParser("%").isSuccess }
  }

  test("Successful parses + operator") {
    assertResult(true) { binaryOp.runParser("+").isSuccess }
  }

  test("Successful parses - operator") {
    assertResult(true) { binaryOp.runParser("-").isSuccess }
  }

  test("Successful parses > operator") {
    assertResult(true) { binaryOp.runParser(">").isSuccess }
  }

  test("Successful parses >= operator") {
    assertResult(true) { binaryOp.runParser(">=").isSuccess }
  }

  test("Successful parses < operator") {
    assertResult(true) { binaryOp.runParser("<").isSuccess }
  }

  test("Successful parses <= operator") {
    assertResult(true) { binaryOp.runParser("<=").isSuccess }
  }

  test("Successful parses == operator") {
    assertResult(true) { binaryOp.runParser("==").isSuccess }
  }

  test("Successful parses != operator") {
    assertResult(true) { binaryOp.runParser("!=").isSuccess }
  }

  test("Successful parses && operator") {
    assertResult(true) { binaryOp.runParser("&&").isSuccess }
  }

  test("Successful parses || operator") {
    assertResult(true) { binaryOp.runParser("||").isSuccess }
  }

  test("Successful fails to parse & operator") {
    assertResult(true) { binaryOp.runParser("&").isFailure }
  }

  test("Successful fails to parse | operator") {
    assertResult(true) { binaryOp.runParser("|").isFailure }
  }
}

class BoolLiteral extends AnyFunSuite {
  test("Successfully parses true") {
    assertResult(true) { identifier.runParser("true").isSuccess }
  }

  test("Successfully parses false") {
    assertResult(true) { identifier.runParser("false").isSuccess }
  }
}

class Identifier extends AnyFunSuite {
  test("Successfully parses simple letter identifiers") {
    assertResult(true) { identifier.runParser("x").isSuccess }
    assertResult(true) { identifier.runParser("abc").isSuccess }
    assertResult(true) { identifier.runParser("xyz").isSuccess }
    assertResult(true) { identifier.runParser("ABC").isSuccess }
  }

  test("Successfully parses simple alphanumeric identifiers") {
    assertResult(true) { identifier.runParser("x1").isSuccess }
    assertResult(true) { identifier.runParser("ABC20").isSuccess }
    assertResult(true) { identifier.runParser("abc123").isSuccess }
  }

  test("Successfully parses simple alphanumeric identifiers with underscore") {
    assertResult(true) { identifier.runParser("_").isSuccess }
    assertResult(true) { identifier.runParser("temp_variable1").isSuccess }
    assertResult(true) { identifier.runParser("x1_y2").isSuccess }
  }

  test("Successfully fails to parses identifiers with underscore") {
    assertResult(true) { identifier.runParser("10x").isFailure }
    assertResult(true) { identifier.runParser("!abc").isFailure }
  }
}

class BaseTypeTest extends AnyFunSuite {
  test("Successfully parses int type") {
    assertResult(true) { baseType.runParser("int").isSuccess }
  }

  test("Successfully parses bool type") {
    assertResult(true) { baseType.runParser("bool").isSuccess }
  }

  test("Successfully parses char type") {
    assertResult(true) { baseType.runParser("char").isSuccess }
  }

  test("Successfully parses string type") {
    assertResult(true) { baseType.runParser("string").isSuccess }
  }

  test("Successfully fails to parse string 'float'") {
    assertResult(true) { unaryOp.runParser("float").isFailure }
  }
}

class EscapedCharTest extends AnyFunSuite {
  test("Successfully parses character 0") {
    assertResult(true) { escapedChar.runParser("0").isSuccess }
  }

  test("Successfully parses character b") {
    assertResult(true) { escapedChar.runParser("b").isSuccess }
  }

  test("Successfully parses character t") {
    assertResult(true) { escapedChar.runParser("t").isSuccess }
  }

  test("Successfully parses character n") {
    assertResult(true) { escapedChar.runParser("n").isSuccess }
  }

  test("Successfully parses character f") {
    assertResult(true) { escapedChar.runParser("f").isSuccess }
  }

  test("Successfully parses character r") {
    assertResult(true) { escapedChar.runParser("r").isSuccess }
  }

  test("Successfully parses character '\"' ") {
    assertResult(true) { escapedChar.runParser("\"").isSuccess }
  }

  test("Successfully parses character '") {
    assertResult(true) { escapedChar.runParser("\'").isSuccess }
  }

  test("Successfully parses character '\' ") {
    assertResult(true) { escapedChar.runParser("\\").isSuccess }
  }
}

class CharacterTest extends AnyFunSuite {
  test("Successfully parses escaped b character") {
    assertResult(true) { character.runParser("\\b").isSuccess }
  }

  test("Successfully fails to parse '\\' character") {
    assertResult(true) { character.runParser("\\").isFailure }
  }

  test("Successfully parses the character 'a'") {
    assertResult(true) { character.runParser("a").isSuccess }
  }
}

class CharLitTest extends AnyFunSuite {
  test("Successfully parses escaped b char literal") {
    assertResult(true) { charLiteral.runParser("'\\b'").isSuccess }
  }

  test("Successfully fails to parse '\\' char literal") {
    assertResult(true) { charLiteral.runParser("'\\'").isFailure }
  }

  test("Successfully parses the char literal 'a'") {
    assertResult(true) { charLiteral.runParser("'a'").isSuccess }
  }
}

class StringLitTest extends AnyFunSuite {
  test("Successfully parses string literal \"hello world\"") {
    assertResult(true) { strLiteral.runParser("\"hello world\"").isSuccess }
  }

  test("Successfully fails to parse \"Joe's\" string literal") {
    assertResult(true) { strLiteral.runParser("\"Joe's\"").isFailure }
  }

  test("Successfully parses string literal \"\\thello world\"") {
    assertResult(true) { strLiteral.runParser("\"\\thello world\"").isSuccess }
  }
}
