import org.scalatest.funsuite.AnyFunSuite
import frontend.Parser._
import frontend.Rules._
import frontend.Lexer._

class ParserFuncTest extends AnyFunSuite {
  test("Successfully parses func with no param-list") {
    assert(
      func
        .runParser("int foo () is return 10 end")
        .contains(
          Func(IntT, Ident("foo", (1, 5)), None, Return(IntLiter(10, (1, 22))))
        )
    )
    assert(
      func
        .runParser("int foo () is x = x + 1 ; exit 0 end")
        .contains(
          Func(
            IntT,
            Ident("foo", (1, 5)),
            None,
            Seq(
              List(
                EqAssign(
                  Ident("x", (1, 15)),
                  Plus(Ident("x", (1, 19)), IntLiter(1, (1, 23)), (1, 21))
                ),
                Exit(IntLiter(0, (1, 32)))
              )
            )
          )
        )
    )
    assert(
      func
        .runParser("int[] foo () is y = x + 1 ; return y end")
        .contains(
          Func(
            ArrayT(IntT),
            Ident("foo", (1, 7)),
            None,
            Seq(
              List(
                EqAssign(
                  Ident("y", (1, 17)),
                  Plus(Ident("x", (1, 21)), IntLiter(1, (1, 25)), (1, 23))
                ),
                Return(Ident("y", (1, 36)))
              )
            )
          )
        )
    )
  }

  test("Successfully parses func with param-list") {
    assert(
      func
        .runParser("int foo (int arg1) is return arg1 end")
        .contains(
          Func(
            IntT,
            Ident("foo", (1, 5)),
            Some(ParamList(List(Param(IntT, Ident("arg1", (1, 14)))))),
            Return(Ident("arg1", (1, 30)))
          )
        )
    )
    assert(
      func
        .runParser("int foo (int arg1, char arg2) is return y end")
        .contains(
          Func(
            IntT,
            Ident("foo", (1, 5)),
            Some(
              ParamList(
                List(
                  Param(IntT, Ident("arg1", (1, 14))),
                  Param(CharT, Ident("arg2", (1, 25)))
                )
              )
            ),
            Return(Ident("y", (1, 41)))
          )
        )
    )
    assert(
      func
        .runParser("string foo (int[] arg1) is exit 1 end")
        .contains(
          Func(
            StringT,
            Ident("foo", (1, 18)),
            Some(ParamList(List(Param(ArrayT(IntT), Ident("arg1", (1, 19)))))),
            Exit(IntLiter(1, (1, 33)))
          )
        )
    )
  }

  test(
    "Successfully fails to parse functions that don't end in 'return' or 'exit'"
  ) {
    assert(
      func
        .runParser("int foo () is skip end")
        .isFailure
    )
    assert(
      func
        .runParser("bool foo () is print 10 end")
        .isFailure
    )
    assert(
      func
        .runParser("char foo () is free var end")
        .isFailure
    )
    assert(
      func
        .runParser("string foo () is x = var end")
        .isFailure
    )
  }
}
