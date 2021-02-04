import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._
import parsley.Success

class UnaryOpTest extends AnyFunSuite {
  test("Successful parses not operator") {
    assert(unaryOp.runParser("!").contains(Not))
  }

  test("Successful parses negation operator") {
    assert(unaryOp.runParser("-").contains(Negation))
  }

  test("Successful parses len operator") {
    assert(unaryOp.runParser("len").contains(Len))
  }

  test("Successful parses ord operator") {
    assert(unaryOp.runParser("ord").contains(Ord))
  }

  test("Successful parses chr operator") {
    assert(unaryOp.runParser("chr").contains(Chr))
  }

  test("Successfully fails to parse random string '##!!!'") {
    assert(unaryOp.runParser("##!!!").isFailure)
  }

  test("Successfully fails to parse random string 'l3n'") {
    assert(unaryOp.runParser("l3n").isFailure)
  }
}

class BinaryOpTest extends AnyFunSuite {
  test("Successful parses * operator") {
    assert(binaryOp.runParser("*").contains(Mul))
  }

  test("Successful parses / operator") {
    assert(binaryOp.runParser("/").contains(Div))
  }

  test("Successful parses % operator") {
    assert(binaryOp.runParser("%").contains(Mod))
  }

  test("Successful parses + operator") {
    assert(binaryOp.runParser("+").contains(Plus))
  }

  test("Successful parses - operator") {
    assert(binaryOp.runParser("-").contains(Sub))
  }

  test("Successful parses > operator") {
    assert(binaryOp.runParser(">").contains(GT))
  }

  test("Successful parses >= operator") {
    assert(binaryOp.runParser(">=").contains(GTE))
  }

  test("Successful parses < operator") {
    assert(binaryOp.runParser("<").contains(LT))
  }

  test("Successful parses <= operator") {
    assert(binaryOp.runParser("<=").contains(LTE))
  }

  test("Successful parses == operator") {
    assert(binaryOp.runParser("==").contains(Equal))
  }

  test("Successful parses != operator") {
    assert(binaryOp.runParser("!=").contains(NotEqual))
  }

  test("Successful parses && operator") {
    assert(binaryOp.runParser("&&").contains(And))
  }

  test("Successful parses || operator") {
    assert(binaryOp.runParser("||").contains(Or))
  }

  test("Successful fails to parse & operator") {
    assert(binaryOp.runParser("&").isFailure)
  }

  test("Successful fails to parse | operator") {
    assert(binaryOp.runParser("|").isFailure)
  }
}

class IntLiter extends AnyFunSuite {
  test("Successfully parses digit without sign") {
    assert(
      intLiter.runParser("100").contains(IntLiter(None, 100))
    )
    assert(intLiter.runParser("1").contains(IntLiter(None, 1)))
  }

  test("Successfully parses digit with sign") {
    assert(
      intLiter.runParser("+100").contains(IntLiter(Some(Pos), 100))
    )
    assert(
      intLiter.runParser("-100").contains(IntLiter(Some(Neg), 100))
    )
  }

  test("Successfully fails to parse sign without digit") {
    assertResult(true) { intLiter.runParser("+").isFailure }
    assertResult(true) { intLiter.runParser("-").isFailure }
  }

  test("Successfully fails to parse number with decimal part") {
    assertResult(true) {
      intLiter.runParser("+100.5").contains(IntLiter(Some(Pos), 100))
    }
    assertResult(true) {
      intLiter.runParser("-0.5").contains(IntLiter(Some(Neg), 0))
    }
  }
}

class NaturalTest extends AnyFunSuite {
  test("Successfully parses natural numbers") {
    assert(natural.runParser("100").contains(100))
    assert(natural.runParser("2").contains(2))
    assert(natural.runParser("0").contains(0))
    assert(natural.runParser("400").contains(400))
  }

  test("Successfully fails to parse decimal part of a number") {
    assert(natural.runParser("100.5").contains(100))
    assert(natural.runParser("0.5").contains(0))
  }
}

class IntSignTest extends AnyFunSuite {
  test("Successfully parses +") {
    assert(intSign.runParser("+").contains(Pos))
  }

  test("Successfully parses -") {
    assert(intSign.runParser("-").contains(Neg))
  }
}

class BoolLiteralTest extends AnyFunSuite {
  test("Successfully parses true") {
    assert(boolLiteral.runParser("true").contains(BoolLiter(true)))
  }

  test("Successfully parses false") {
    assert(boolLiteral.runParser("false").contains(BoolLiter(false)))
  }
}

class IdentifierTest extends AnyFunSuite {
  test("Successfully parses simple letter identifiers") {
    assert(identifier.runParser("x").contains(Ident("x")))
    assert(identifier.runParser("abc").contains(Ident("abc")))
    assert(identifier.runParser("xyz").contains(Ident("xyz")))
    assert(identifier.runParser("ABC").contains(Ident("ABC")))
  }

  test("Successfully parses simple alphanumeric identifiers") {
    assert(identifier.runParser("x1").contains(Ident("x1")))
    assert(identifier.runParser("ABC20").contains(Ident("ABC20")))
    assert(identifier.runParser("abc123").contains(Ident("abc123")))
  }

  test("Successfully parses simple alphanumeric identifiers with underscore") {
    assert(identifier.runParser("_").contains(Ident("_")))
    assert(
      identifier.runParser("temp_variable1").contains(Ident("temp_variable1"))
    )
    assert(identifier.runParser("x1_y2").contains(Ident("x1_y2")))
  }

  test("Successfully fails to parse identifiers with underscore") {
    assert(identifier.runParser("10x").isFailure)
    assert(identifier.runParser("!abc").isFailure)
  }
}

class BaseTypeTest extends AnyFunSuite {
  test("Successfully parses int type") {
    assert(baseType.runParser("int").contains(IntT))
  }

  test("Successfully parses bool type") {
    assert(baseType.runParser("bool").contains(BoolT))
  }

  test("Successfully parses char type") {
    assert(baseType.runParser("char").contains(CharT))
  }

  test("Successfully parses string type") {
    assert(baseType.runParser("string").contains(StringT))
  }

  test("Successfully fails to parse string 'float'") {
    assert(baseType.runParser("float").isFailure)
  }
}

class EscapedCharTest extends AnyFunSuite {
  test("Successfully parses character 0") {
    assert(escapedChar.runParser("0").contains('0'))
  }

  test("Successfully parses character b") {
    assert(escapedChar.runParser("b").contains('b'))
  }

  test("Successfully parses character t") {
    assert(escapedChar.runParser("t").contains('t'))
  }

  test("Successfully parses character n") {
    assert(escapedChar.runParser("n").contains('n'))
  }

  test("Successfully parses character f") {
    assert(escapedChar.runParser("f").contains('f'))
  }

  test("Successfully parses character r") {
    assert(escapedChar.runParser("r").contains('r'))
  }

  test("Successfully parses character '\"' ") {
    assert(escapedChar.runParser("\"").contains('\"'))
  }

  test("Successfully parses character '") {
    assert(escapedChar.runParser("\'").contains('\''))
  }

  test("Successfully parses character '\' ") {
    assert(escapedChar.runParser("\\").contains('\\'))
  }
}

class CharacterTest extends AnyFunSuite {
  test("Successfully parses escaped b character") {
    assert(character.runParser("\\b").contains(Escape('b')))
  }

  test("Successfully fails to parse '\\' character") {
    assert(character.runParser("\\").isFailure)
  }

  test("Successfully parses the character 'a'") {
    assert(character.runParser("a").contains(NormalChar('a')))
  }
}

class CharLitTest extends AnyFunSuite {
  test("Successfully parses escaped b char literal") {
    assert(charLiteral.runParser("'\\b'").contains(CharLiter(Escape('b'))))
  }

  test("Successfully fails to parse '\\' char literal") {
    assert(charLiteral.runParser("'\\'").isFailure)
  }

  test("Successfully parses the char literal 'a'") {
    assert(charLiteral.runParser("'a'").contains(CharLiter(NormalChar('a'))))
  }
}

class StringLitTest extends AnyFunSuite {
  test("Successfully parses string literal \"hello world\"") {
    assert(strLiteral.runParser("\"hello world\"").isSuccess)
  }

  test("Successfully fails to parse \"Joe's\" string literal") {
    assert(strLiteral.runParser("\"Joe's\"").isFailure)
  }

  test("Successfully parses string literal \"\\thello world\"") {
    assert(strLiteral.runParser("\"\\thello world\"").isSuccess)
  }
}

class ArrayTypeTest extends AnyFunSuite {
  test("Successfully parses base type arrays") {
    assert(types.runParser("int[]").contains(OfArrayType(IntT)))
    assert(types.runParser("string[]").contains(OfArrayType(StringT)))
    assert(types.runParser("char[]").contains(OfArrayType(CharT)))
    assert(types.runParser("bool[]").contains(OfArrayType(BoolT)))
  }

  test("Successfully parses nested arrays") {
    assert(types.runParser("int[][]").contains(OfArrayType(OfArrayType(IntT))))
    assert(
      types
        .runParser("bool[][][]")
        .contains(OfArrayType(OfArrayType(OfArrayType(BoolT))))
    )
  }
}
