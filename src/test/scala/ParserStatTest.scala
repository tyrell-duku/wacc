import org.scalatest.funsuite.AnyFunSuite
import Parser._
import Rules._
import LiterParser._
import ExprParser._
import StatParser._
import Lexer._
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
        .contains(Return(Parens(IntLiter(0))))
    )
  }

  test("Successfully parses exit statement") {
    assert(
      stat.runParser("exit(5)").contains(Exit(Parens(IntLiter(5))))
    )
  }

  test("Successfully parses free statement") {
    assert(
      stat
        .runParser("free(100)")
        .contains(Free(Parens(IntLiter(100))))
    )
  }

  test("Successfully parses if statement, with whitespace") {
    assert(
      statWhitespace
        .runParser("if (100 == 5) then print 100 else print 5 fi")
        .contains(
          If(
            Parens(Equal(IntLiter(100), IntLiter(5))),
            Print(IntLiter(100)),
            Print(IntLiter(5))
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
            While(BoolLiter(true), Print(Plus(Ident("x"), IntLiter(1))))
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
            List(
              While(BoolLiter(true), Print(Ident("x"))),
              Begin(Print(IntLiter(5)))
            )
          )
        )
    )
  }

  test("Successfully fails to parse invalid if statement") {
    assert(
      statWhitespace
        .runParser("if 10 == 11 then print \"invalid\" else skip")
        .isFailure
    )
    assert(
      statWhitespace
        .runParser("if var print \"invalid\" else skip fi")
        .isFailure
    )
    assert(
      statWhitespace
        .runParser("if var then print \"invalid\" skip fi")
        .isFailure
    )
  }

  test("Successfully fails to parse invalid while statement") {
    assert(statWhitespace.runParser("while var print 5 done").isFailure)
    assert(statWhitespace.runParser("while var do print 5").isFailure)
    assert(statWhitespace.runParser("while var do print 5").isFailure)
  }

  test("Successfully fails to parse invalid begin statement") {
    assert(statWhitespace.runParser("begin skip").isFailure)
  }

  test("Successfully parses assignLHS = assignRHS") {
    assert(
      statWhitespace
        .runParser("x = x + 1")
        .contains(EqAssign(Ident("x"), Plus(Ident("x"), IntLiter(1))))
    )
  }

  test("Successfully parses identifier assignment") {
    assert(
      statWhitespace
        .runParser("int var = 1")
        .contains(EqIdent(IntT, Ident("var"), IntLiter(1)))
    )
  }

  test("Successfully parses nested statements") {
    assert(
      statWhitespace
        .runParser("while x < y do x = x + 1 ; y = y + 1 done")
        .contains(
          While(
            LT(Ident("x"), Ident("y")),
            Seq(
              List(
                EqAssign(Ident("x"), Plus(Ident("x"), IntLiter(1))),
                EqAssign(Ident("y"), Plus(Ident("y"), IntLiter(1)))
              )
            )
          )
        )
    )
    assert(
      statWhitespace
        .runParser(
          "if var1 then if var2 then return var2 else skip fi else return var1 fi"
        )
        .contains(
          If(
            Ident("var1"),
            If(Ident("var2"), Return(Ident("var2")), Skip),
            Return(Ident("var1"))
          )
        )
    )
  }
}
