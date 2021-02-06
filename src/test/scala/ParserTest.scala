import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._
import parsley.Success
import parsley.character.whitespace
import parsley.token.{LanguageDef, Lexer, Parser}

class UnaryOpTest extends AnyFunSuite {

  test("Successful parses not operator") {
    assert(expr.runParser("!var").contains(Not(Ident("var"))))
  }

  test("Successful parses negation operator") {
    assert(expr.runParser("-var").contains(Negation(Ident("var"))))
  }

  test("Successful parses len operator") {
    assert(expr.runParser("len\"hello\"").isSuccess)
  }

  test("Successful parses ord operator") {
    assert(expr.runParser("ord'c'").contains(Ord(CharLiter(NormalChar('c')))))
  }

  test("Successful parses chr operator") {
    assert(expr.runParser("chr10").contains(Chr(IntLiter(None, 10))))
  }

  test("Successfully fails to parse random string '##!!!'") {
    assert(expr.runParser("##!!!").isFailure)
  }

  test("Successfully fails to parse random string 'l3n'") {
    assert(expr.runParser("l3n").isFailure)
  }
}

class BinaryOpTest extends AnyFunSuite {
  test("Successful parses * operator") {
    assert(
      expr
        .runParser("10*5")
        .contains(Mul(IntLiter(None, 10), IntLiter(None, 5)))
    )
  }

  test("Successful parses / operator") {
    assert(
      expr.runParser("4/2").contains(Div(IntLiter(None, 4), IntLiter(None, 2)))
    )
  }

  test("Successful parses % operator") {
    assert(
      expr.runParser("3%1").contains(Mod(IntLiter(None, 3), IntLiter(None, 1)))
    )
  }

  test("Successful parses + operator") {
    assert(
      expr
        .runParser("14+5")
        .contains(Plus(IntLiter(None, 14), IntLiter(None, 5)))
    )
  }

  test("Successful parses - operator") {
    assert(
      expr
        .runParser("6-18")
        .contains(Sub(IntLiter(None, 6), IntLiter(None, 18)))
    )
  }

  test("Successful parses > operator") {
    assert(
      expr.runParser("10>5").contains(GT(IntLiter(None, 10), IntLiter(None, 5)))
    )
  }

  test("Successful parses >= operator") {
    assert(
      expr.runParser("3>=7").contains(GTE(IntLiter(None, 3), IntLiter(None, 7)))
    )
  }

  test("Successful parses < operator") {
    assert(
      expr.runParser("6<4").contains(LT(IntLiter(None, 6), IntLiter(None, 4)))
    )
  }

  test("Successful parses <= operator") {
    assert(
      expr.runParser("6<=1").contains(LTE(IntLiter(None, 6), IntLiter(None, 1)))
    )
  }

  test("Successful parses == operator") {
    assert(
      expr.runParser("true==var").contains(Equal(BoolLiter(true), Ident("var")))
    )
  }

  test("Successful parses != operator") {
    assert(
      expr
        .runParser("false!=var")
        .contains(NotEqual(BoolLiter(false), Ident("var")))
    )
  }

  test("Successful parses && operator") {
    assert(
      expr
        .runParser("true&&true")
        .contains(And(BoolLiter(true), BoolLiter(true)))
    )
  }

  test("Successful parses || operator") {
    assert(
      expr
        .runParser("false||true")
        .contains(Or(BoolLiter(false), BoolLiter(true)))
    )
  }

  test("Successful fails to parse & operator") {
    assert(expr.runParser("&").isFailure)
  }

  test("Successful fails to parse | operator") {
    assert(expr.runParser("|").isFailure)
  }
}

class ExprTest extends AnyFunSuite {
  test("Successfully parses expr within parentheses") {
    assert(
      expr
        .runParser("((10))+(5)")
        .contains(
          Plus(Parens(Parens(IntLiter(None, 10))), Parens(IntLiter(None, 5)))
        )
    )
  }

  test("Successfully parses nested unary operations") {
    assert(
      expr
        .runParser("ord(chr(10))")
        .contains(Ord(Parens(Chr(Parens(IntLiter(None, 10))))))
    )
  }

  test("Successfully parses bool operators in correct precedence") {
    assert(
      expr
        .runParser("!(!true||false)")
        .contains(Not(Parens(Or(Not(BoolLiter(true)), BoolLiter(false)))))
    )
  }

  test("Successfully parses arithmetic operations in correct precedence") {
    assert(
      expr
        .runParser("2*8+5/9-1")
        .contains(
          Sub(
            Plus(
              Mul(IntLiter(None, 2), IntLiter(None, 8)),
              Div(IntLiter(None, 5), IntLiter(None, 9))
            ),
            IntLiter(None, 1)
          )
        )
    )
    assert(
      expr
        .runParser("100+8*5/9-1")
        .contains(
          Sub(
            Plus(
              IntLiter(None, 100),
              Div(Mul(IntLiter(None, 8), IntLiter(None, 5)), IntLiter(None, 9))
            ),
            IntLiter(None, 1)
          )
        )
    )
    assert(
      expr
        .runParser("100+8*5+100-1")
        .contains(
          Sub(
            Plus(
              Plus(
                IntLiter(None, 100),
                Mul(IntLiter(None, 8), IntLiter(None, 5))
              ),
              IntLiter(None, 100)
            ),
            IntLiter(None, 1)
          )
        )
    )
  }

  test("Successfully fails parsing empty parenthesis ") {
    assert(expr.runParser("()").isFailure)
  }

  test("Successfully parses binary operators on logical atoms") {
    assert(
      expr
        .runParser("var1==(ord('c'))")
        .contains(
          Equal(Ident("var1"), Parens(Ord(Parens(CharLiter(NormalChar('c'))))))
        )
    )
  }
}

class IntLiterTest extends AnyFunSuite {
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
    assertResult(true) {
      intLiter.runParser("+").isFailure
    }
    assertResult(true) {
      intLiter.runParser("-").isFailure
    }
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

class PairTypeTest extends AnyFunSuite {
  test("Successfully parses pair of base types") {
    assert(
      types
        .runParser("pair(int,int)")
        .contains(Pair(PairElemT(IntT), PairElemT(IntT)))
    )
    assert(
      types
        .runParser("pair(char,int)")
        .contains(Pair(PairElemT(CharT), PairElemT(IntT)))
    )
  }

  test("Successfully parses pair with array-types") {
    assert(
      types
        .runParser("pair(int[],int[])")
        .contains(
          Pair(PairElemT(OfArrayType(IntT)), PairElemT(OfArrayType(IntT)))
        )
    )
    assert(
      types
        .runParser("pair(bool[],int[])")
        .contains(
          Pair(PairElemT(OfArrayType(BoolT)), PairElemT(OfArrayType(IntT)))
        )
    )
    assert(
      types
        .runParser("pair(bool[],int[])")
        .contains(
          Pair(PairElemT(OfArrayType(BoolT)), PairElemT(OfArrayType(IntT)))
        )
    )
    assert(
      types
        .runParser("pair(bool[][],int[])")
        .contains(
          Pair(
            PairElemT(OfArrayType(OfArrayType(BoolT))),
            PairElemT(OfArrayType(IntT))
          )
        )
    )
    assert(
      types
        .runParser("pair(char[][],int[])[]")
        .contains(
          OfArrayType(
            Pair(
              PairElemT(OfArrayType(OfArrayType(CharT))),
              PairElemT(OfArrayType(IntT))
            )
          )
        )
    )
  }

  test("Successfully fails to parse pair-type within pair-type") {
    assert(types.runParser("pair(int,pair(char,int))").isFailure)
  }

  test("Successfully parses 'pair' within pair-type") {
    assert(
      types
        .runParser("pair(pair,pair)")
        .contains(Pair(PairElemPair, PairElemPair))
    )
  }
}

class ArrayElemTest extends AnyFunSuite {
  test("Successfully parses double array elem") {
    assert(
      arrayElem
        .runParser("var[10][2]")
        .contains(
          ArrayElem(Ident("var"), List(IntLiter(None, 10), IntLiter(None, 2)))
        )
    )
  }

  test("Successfully fails to parse an empty arrayElem") {
    assert(arrayElem.runParser("var[]").isFailure)
  }

  test("Successfully parses single element arrayElem") {
    assert(
      arrayElem
        .runParser("var[10]")
        .contains(ArrayElem(Ident("var"), List(IntLiter(None, 10))))
    )
    assert(
      arrayElem
        .runParser("var[var[10]]")
        .contains(
          ArrayElem(Ident("var"),
            List(ArrayElem(Ident("var"), List(IntLiter(None, 10)))))
        )
    )

  }
}

class ArgListTest extends AnyFunSuite {
  test("Successfully parses arguments") {
    assert(
      argList
        .runParser("'c','v','g'")
        .contains(
          ArgList(
            List(
              CharLiter(NormalChar('c')),
              CharLiter(NormalChar('v')),
              CharLiter(NormalChar('g'))
            )
          )
        )
    )
    assert(
      argList
        .runParser("10,2,3")
        .contains(
          ArgList(
            List(IntLiter(None, 10), IntLiter(None, 2), IntLiter(None, 3))
          )
        )
    )
    assert(
      argList
        .runParser("arg1,arg2,arg3")
        .contains(ArgList(List(Ident("arg1"), Ident("arg2"), Ident("arg3"))))
    )
  }

  test("Successfully fails to parse an empty argList") {
    assert(argList.runParser("").isFailure)
  }

  test("Successfully parses single element argList") {
    assert(argList.runParser("10").contains(ArgList(List(IntLiter(None, 10)))))
    assert(
      argList
        .runParser("'c''")
        .contains(ArgList(List(CharLiter(NormalChar('c')))))
    )
    assert(
      argList
        .runParser("10*6")
        .contains(ArgList(List(Mul(IntLiter(None, 10), IntLiter(None, 6)))))
    )
    assert(argList.runParser("var[10]").contains(ArgList(List(ArrayElem(Ident("var"), List(IntLiter(None, 10)))))))
  }
}

class StatTest extends AnyFunSuite {
  test("Successfully parses skip statements") {
    assert(statement.runParser("skip").contains(Skip))
  }

  test("Successfully parses print statements") {
    assert(statement.runParser("print(a+y)").contains(Print(Parens(Plus(Ident
    ("a"), Ident("y"))
    ))))
  }

  test("Successfully parses println statements") {
    assert(statement.runParser("println(a)").contains(PrintLn(Parens(Ident
    ("a")))))
  }
  test("Successfully parses return statements") {
    assert(statement.runParser("return(0)").contains(Return(Parens(IntLiter
    (None, 0)))))
  }
  test("Successfully parses exit statements") {
    assert(statement.runParser("exit(5)").contains(Exit(Parens(IntLiter(None,
      5)))))
  }
  test("Successfully parses free statements") {
    assert(statement.runParser("free(100)").contains(Free(Parens(IntLiter
    (None, 100)))))
  }

}

class PairElemTest extends AnyFunSuite {
  test("Successfully parses fst statements") {
    assert(pairElem.runParser("fst(65)").contains(Fst(Parens(IntLiter(None, 65)
    ))))
  }
  test("Successfully fails to parse fst statements w/ non-exp parameter") {
    assert(pairElem.runParser("fst(fst(90))").isFailure)
  }

  test("Successfully parses snd statements") {
    assert(pairElem.runParser("snd(5)").contains(Snd(Parens(IntLiter(None, 5)
    ))))
  }
  test("Successfully fails to parse snd statements w/ non-exp parameter") {
    assert(pairElem.runParser("fst(snd(fst(90)))").isFailure)
  }

}

class AssignRHSTests extends AnyFunSuite {
  test("Successfully parses function calls with one argument") {
    assert(assignRHS.runParser("call p(q)").contains(Call(Ident("p"), Some
    (ArgList(List(Ident("q")))))))
  }
  test("Successfully parses function calls with multiple arguments") {
    assert(assignRHS.runParser("call p(q,r,s)").contains(Call(Ident("p"), Some
    (ArgList(List(Ident("q"), Ident("r"), Ident("s")))))))
  }

  test("Successfully parses function calls with no arguments") {
    assert(assignRHS.runParser("call p()").contains(Call(Ident("p"), None)))
  }

  test("Successfully fails to parse embedded function calls") {
    assert(assignRHS.runParser("call p(call q(call r()))").isFailure)
  }

}



