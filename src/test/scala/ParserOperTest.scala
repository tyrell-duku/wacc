import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._
import LiterParser._
import StatParser._
import ExprParser._
import Lexer._

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
    assert(expr.runParser("ord 'c'").contains(Ord(CharLiter(NormalChar('c')))))
  }

  test("Successful parses chr operator") {
    assert(expr.runParser("chr 10").contains(Chr(IntLiter(10))))
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
        .contains(Mul(IntLiter(10), IntLiter(5)))
    )
  }

  test("Successful parses / operator") {
    assert(
      expr.runParser("4/2").contains(Div(IntLiter(4), IntLiter(2)))
    )
  }

  test("Successful parses % operator") {
    assert(
      expr.runParser("3%1").contains(Mod(IntLiter(3), IntLiter(1)))
    )
  }

  test("Successful parses + operator") {
    assert(
      expr
        .runParser("14+5")
        .contains(Plus(IntLiter(14), IntLiter(5)))
    )
  }

  test("Successful parses - operator") {
    assert(
      expr
        .runParser("6-18")
        .contains(Sub(IntLiter(6), IntLiter(18)))
    )
  }

  test("Successful parses > operator") {
    assert(
      expr.runParser("10>5").contains(GT(IntLiter(10), IntLiter(5)))
    )
  }

  test("Successful parses >= operator") {
    assert(
      expr.runParser("3>=7").contains(GTE(IntLiter(3), IntLiter(7)))
    )
  }

  test("Successful parses < operator") {
    assert(
      expr.runParser("6<4").contains(LT(IntLiter(6), IntLiter(4)))
    )
  }

  test("Successful parses <= operator") {
    assert(
      expr.runParser("6<=1").contains(LTE(IntLiter(6), IntLiter(1)))
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

  test("Successful fails to parse invalid & operator") {
    assert(expr.runParser("&").isFailure)
  }

  test("Successful fails to parse invalid | operator") {
    assert(expr.runParser("|").isFailure)
  }
}
