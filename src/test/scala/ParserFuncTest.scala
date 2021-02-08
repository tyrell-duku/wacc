import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._

class FuncTest extends AnyFunSuite {
  test("Successfully parses func with no param-list") {
    assert(
      func
        .runParser("int foo () is skip end")
        .contains(Func(IntT, Ident("foo"), None, Skip))
    )
    assert(
      func
        .runParser("bool foo () is skip end")
        .contains(Func(BoolT, Ident("foo"), None, Skip))
    )
    assert(
      func
        .runParser("char foo () is skip end")
        .contains(Func(CharT, Ident("foo"), None, Skip))
    )
    assert(
      func
        .runParser("string foo () is skip end")
        .contains(Func(StringT, Ident("foo"), None, Skip))
    )
    assert(
      func
        .runParser("int foo () is return x end")
        .contains(Func(IntT, Ident("foo"), None, Return(Ident("x"))))
    )
    assert(
      func
        .runParser("int foo () is x = x + 1 end")
        .contains(
          Func(
            IntT,
            Ident("foo"),
            None,
            EqAssign(Ident("x"), Plus(Ident("x"), IntLiter(1)))
          )
        )
    )
    assert(
      func
        .runParser("int[] foo () is y = x + 1 end")
        .contains(
          Func(
            OfArrayType(IntT),
            Ident("foo"),
            None,
            EqAssign(Ident("y"), Plus(Ident("x"), IntLiter(1)))
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
}
