import org.scalatest.funsuite.AnyFunSuite
import frontend.Rules._
import frontend.ExprParser._
import frontend.Lexer._

class UnaryOpTest extends AnyFunSuite {

  test("Successful parses not operator") {
    assert(expr.runParser("!var").contains(Not(Ident("var", (1, 2)), (1, 1))))
  }

  test("Successful parses negation operator") {
    assert(
      expr.runParser("-var").contains(Negation(Ident("var", (1, 2)), (1, 1)))
    )
  }

  test("Successful parses len operator") {
    assert(expr.runParser("len\"hello\"").isSuccess)
  }

  test("Successful parses ord operator") {
    assert(
      expr
        .runParser("ord 'c'")
        .contains(Ord(CharLiter(NormalChar('c'), (1, 5)), (1, 1)))
    )
  }

  test("Successful parses chr operator") {
    assert(expr.runParser("chr 10").contains(Chr(IntLiter(10, (1, 5)), (1, 1))))
  }

  test("Successfully fails to parse random string '##!!!'") {
    assert(expr.runParser("##!!!").isFailure)
  }
}

class BinaryOpTest extends AnyFunSuite {
  test("Successful parses * operator") {
    assert(
      expr
        .runParser("10*5")
        .contains(Mul(IntLiter(10, (1, 1)), IntLiter(5, (1, 4)), (1, 3)))
    )
  }

  test("Successful parses / operator") {
    assert(
      expr
        .runParser("4/2")
        .contains(Div(IntLiter(4, (1, 1)), IntLiter(2, (1, 3)), (1, 2)))
    )
  }

  test("Successful parses % operator") {
    assert(
      expr
        .runParser("3%1")
        .contains(Mod(IntLiter(3, (1, 1)), IntLiter(1, (1, 3)), (1, 2)))
    )
  }

  test("Successful parses + operator") {
    assert(
      expr
        .runParser("14+5")
        .contains(Plus(IntLiter(14, (1, 1)), IntLiter(5, (1, 4)), (1, 3)))
    )
  }

  test("Successful parses - operator") {
    assert(
      expr
        .runParser("6-18")
        .contains(Sub(IntLiter(6, (1, 1)), IntLiter(18, (1, 3)), (1, 2)))
    )
  }

  test("Successful parses > operator") {
    assert(
      expr
        .runParser("10>5")
        .contains(GT(IntLiter(10, (1, 1)), IntLiter(5, (1, 4)), (1, 3)))
    )
  }

  test("Successful parses >= operator") {
    assert(
      expr
        .runParser("3>=7")
        .contains(GTE(IntLiter(3, (1, 1)), IntLiter(7, (1, 4)), (1, 2)))
    )
  }

  test("Successful parses < operator") {
    assert(
      expr
        .runParser("6<4")
        .contains(LT(IntLiter(6, (1, 1)), IntLiter(4, (1, 3)), (1, 2)))
    )
  }

  test("Successful parses <= operator") {
    assert(
      expr
        .runParser("6<=1")
        .contains(LTE(IntLiter(6, (1, 1)), IntLiter(1, (1, 4)), (1, 2)))
    )
  }

  test("Successful parses == operator") {
    assert(
      expr
        .runParser("true==var")
        .contains(Equal(BoolLiter(true, (1, 1)), Ident("var", (1, 7)), (1, 5)))
    )
  }

  test("Successful parses != operator") {
    assert(
      expr
        .runParser("false!=var")
        .contains(
          NotEqual(BoolLiter(false, (1, 1)), Ident("var", (1, 8)), (1, 6))
        )
    )
  }

  test("Successful parses && operator") {
    assert(
      expr
        .runParser("true&&true")
        .contains(And(BoolLiter(true, (1, 1)), BoolLiter(true, (1, 7)), (1, 5)))
    )
  }

  test("Successful parses || operator") {
    assert(
      expr
        .runParser("false||true")
        .contains(Or(BoolLiter(false, (1, 1)), BoolLiter(true, (1, 8)), (1, 6)))
    )
  }

  test("Successful fails to parse invalid & operator") {
    assert(expr.runParser("&").isFailure)
  }

  test("Successful fails to parse invalid | operator") {
    assert(expr.runParser("|").isFailure)
  }
}
