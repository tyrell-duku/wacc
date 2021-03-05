import org.scalatest.funsuite.AnyFunSuite
import frontend.Rules._
import frontend.StatParser._
import frontend.Lexer._
import parsley.combinator.eof

class ParserStatTest extends AnyFunSuite {
  val statWhitespace = lexer.whiteSpace *> stat <* eof

  test("Successfully parses skip statement") {
    assert(stat.runParser("skip").contains(Skip))
  }

  test("Successfully parses print statement") {
    assert(
      stat
        .runParser("print(a+y)")
        .contains(Print(Plus(Ident("a", (1, 7)), Ident("y", (1, 9)), (1, 8))))
    )
  }

  test("Successfully parses println statement") {
    assert(
      stat.runParser("println(a)").contains(PrintLn(Ident("a", (1, 9))))
    )
  }

  test("Successfully parses return statement") {
    assert(
      stat
        .runParser("return(0)")
        .contains(Return(IntLiter(0, (1, 8))))
    )
  }

  test("Successfully parses exit statement") {
    assert(
      stat.runParser("exit(5)").contains(Exit(IntLiter(5, (1, 6))))
    )
  }

  test("Successfully parses free statement") {
    assert(
      stat
        .runParser("free(100)")
        .contains(Free(IntLiter(100, (1, 6))))
    )
  }

  test("Successfully parses if statement, with whitespace") {
    assert(
      statWhitespace
        .runParser("if (100 == 5) then print 100 else print 5 fi")
        .contains(
          If(
            Equal(IntLiter(100, (1, 5)), IntLiter(5, (1, 12)), (1, 9)),
            Print(IntLiter(100, (1, 26))),
            Print(IntLiter(5, (1, 41)))
          )
        )
    )
  }

  test("Successfully parses while statement, with whitespace") {
    assert(
      statWhitespace
        .runParser("while true do skip done")
        .contains(While(BoolLiter(true, (1, 7)), Skip))
    )
  }

  test("Successfully parses begin statement, with whitespace") {
    assert(
      statWhitespace
        .runParser("begin while true do print x + 1 done end")
        .contains(
          Begin(
            While(
              BoolLiter(true, (1, 13)),
              Print(Plus(Ident("x", (1, 27)), IntLiter(1, (1, 31)), (1, 29)))
            )
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
              While(BoolLiter(true, (1, 7)), Print(Ident("x", (1, 21)))),
              Begin(Print(IntLiter(5, (1, 42))))
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
        .contains(
          EqAssign(
            Ident("x", (1, 1)),
            Plus(Ident("x", (1, 5)), IntLiter(1, (1, 9)), (1, 7))
          )
        )
    )
  }

  test("Successfully parses identifier assignment") {
    assert(
      statWhitespace
        .runParser("int var = 1")
        .contains(EqIdent(IntT, Ident("var", (1, 5)), IntLiter(1, (1, 11))))
    )
  }

  test("Successfully parses nested statements") {
    assert(
      statWhitespace
        .runParser("while x < y do x = x + 1 ; y = y + 1 done")
        .contains(
          While(
            LT(Ident("x", (1, 7)), Ident("y", (1, 11)), (1, 9)),
            Seq(
              List(
                EqAssign(
                  Ident("x", (1, 16)),
                  Plus(Ident("x", (1, 20)), IntLiter(1, (1, 24)), (1, 22))
                ),
                EqAssign(
                  Ident("y", (1, 28)),
                  Plus(Ident("y", (1, 32)), IntLiter(1, (1, 36)), (1, 34))
                )
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
            Ident("var1", (1, 4)),
            If(Ident("var2", (1, 17)), Return(Ident("var2", (1, 34))), Skip),
            Return(Ident("var1", (1, 64)))
          )
        )
    )
  }
}
