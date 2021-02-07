import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._
import parsley.combinator.eof

class StatTest extends AnyFunSuite {
  val statWhitespace = lexer.whiteSpace *> stat <* eof

  test("Successfully parses skip statement") {
    assert(stat.runParser("skip").contains(Skip))
  }

  test("Successfully parses print statement") {
    assert(
      stat
        .runParser("print(a+y)")
        .contains(Print(Parens(Plus(Ident("a"), Ident("y")))))
    )
  }

  test("Successfully parses println statement") {
    assert(
      stat.runParser("println(a)").contains(PrintLn(Parens(Ident("a"))))
    )
  }

  test("Successfully parses return statement") {
    assert(
      stat
        .runParser("return(0)")
        .contains(Return(Parens(IntLiter(None, 0))))
    )
  }

  test("Successfully parses exit statement") {
    assert(
      stat.runParser("exit(5)").contains(Exit(Parens(IntLiter(None, 5))))
    )
  }

  test("Successfully parses free statement") {
    assert(
      stat
        .runParser("free(100)")
        .contains(Free(Parens(IntLiter(None, 100))))
    )
  }

  test("Successfully parses if statement, with whitespace") {
    assert(
      statWhitespace
        .runParser("if (100 == 5) then print 100 else print 5 fi")
        .contains(
          If(
            Parens(Equal(IntLiter(None, 100), IntLiter(None, 5))),
            Print(IntLiter(None, 100)),
            Print(IntLiter(None, 5))
          )
        )
    )
  }

  test("Successfully parses while statement, with whitespace") {
    assert(
      statWhitespace
        .runParser("while true do skip done")
        .contains(While(BoolLiter(true), Skip))
    )
  }

  test("Successfully parses begin statement, with whitespace") {
    assert(
      statWhitespace
        .runParser("begin while true do print x + 1 done end")
        .contains(
          Begin(
            While(BoolLiter(true), Print(Plus(Ident("x"), IntLiter(None, 1))))
          )
        )
    )
  }

  test("Successfully parses consecutive statements, with whitespace") {
    assert(
      statWhitespace
        .runParser("while true do print x done ; begin print 5 end")
        .contains(
          Seq(
            While(BoolLiter(true), Print(Ident("x"))),
            Begin(Print(IntLiter(None, 5)))
          )
        )
    )
  }
}
