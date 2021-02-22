import org.scalatest.funsuite.AnyFunSuite
import ExprParser._

import frontend.Rules._
class ArgListTest extends AnyFunSuite {
  test("Successfully parses arguments") {
    assert(
      argList
        .runParser("'c','v','g'")
        .contains(
          ArgList(
            List(
              CharLiter(NormalChar('c'),(1,1)),
              CharLiter(NormalChar('v'),(1,5)),
              CharLiter(NormalChar('g'),(1,9))
            )
          )
        )
    )
    assert(
      argList
        .runParser("10,2,3")
        .contains(
          ArgList(
            List(IntLiter(10, (1,1)), IntLiter(2, (1,4)), IntLiter(3, (1,6)))
          )
        )
    )
    assert(
      argList
        .runParser("arg1,arg2,arg3")
        .contains(ArgList(List(Ident("arg1", (1,1)), Ident("arg2", (1,5)), Ident("arg3", (1,9)))))
    )
  }

  test("Successfully fails to parse an empty argList") {
    assert(argList.runParser("").isFailure)
  }

  test("Successfully parses single element argList") {
    assert(argList.runParser("10").contains(ArgList(List(IntLiter(10, (1,1))))))
    assert(
      argList
        .runParser("'c''")
        .contains(ArgList(List(CharLiter(NormalChar('c'), (1,1)))))
    )
    assert(
      argList
        .runParser("10*6")
        .contains(ArgList(List(Mul(IntLiter(10, (1,1)), IntLiter(6, (1,4)),(1,3)))))
    )
    assert(
      argList
        .runParser("var[10]")
        .contains(
          ArgList(List(ArrayElem(Ident("var", (1,1)), List(IntLiter(10, (1,5))),(1,1))))
        )
    )
  }
}
