import org.scalatest.funsuite.AnyFunSuite
import Parser._
import ExprParser._

import Rules._
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
            List(IntLiter(10), IntLiter(2), IntLiter(3))
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
    assert(argList.runParser("10").contains(ArgList(List(IntLiter(10)))))
    assert(
      argList
        .runParser("'c''")
        .contains(ArgList(List(CharLiter(NormalChar('c')))))
    )
    assert(
      argList
        .runParser("10*6")
        .contains(ArgList(List(Mul(IntLiter(10), IntLiter(6)))))
    )
    assert(
      argList
        .runParser("var[10]")
        .contains(
          ArgList(List(ArrayElem(Ident("var"), List(IntLiter(10)))))
        )
    )
  }
}
