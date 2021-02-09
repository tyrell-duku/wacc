import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._

class FuncTest extends AnyFunSuite {
  test("Successfully parses func with no param-list") {
    assert(
      func
        .runParser("int foo () is return 10 end")
        .contains(Func(IntT, Ident("foo"), None, Return(IntLiter(10))))
    )
    assert(
      func
        .runParser("int foo () is x = x + 1 ; exit 0 end")
        .contains(
          Func(
            IntT,
            Ident("foo"),
            None,
            Seq(EqAssign(Ident("x"),Plus(Ident("x"),IntLiter(1))),Exit(IntLiter(0)))
          )
        )
    )
    assert(
      func
        .runParser("int[] foo () is y = x + 1 ; return y end")
        .contains(
          Func(
            OfArrayType(IntT),
            Ident("foo"),
            None,
            Seq(EqAssign(Ident("y"),Plus(Ident("x"),IntLiter(1))),Return(Ident("y")))
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
            Ident("foo"),
            Some(ParamList(List(Param(IntT, Ident("arg1"))))),
            Return(Ident("arg1"))
          )
        )
    )
    assert(
      func
        .runParser("int foo (int arg1, char arg2) is return y end")
        .contains(
          Func(
            IntT,
            Ident("foo"),
            Some(
              ParamList(
                List(Param(IntT, Ident("arg1")), Param(CharT, Ident("arg2")))
              )
            ),
            Return(Ident("y"))
          )
        )
    )
    assert(
      func
        .runParser("string foo (int[] arg1) is exit 1 end")
        .contains(
          Func(
            StringT,
            Ident("foo"),
            Some(ParamList(List(Param(OfArrayType(IntT), Ident("arg1"))))),
            Exit(IntLiter(1))
          )
        )
    )
  }

  test("Successfully fails to parse functions that don't end in 'return' or 'exit'"){
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
