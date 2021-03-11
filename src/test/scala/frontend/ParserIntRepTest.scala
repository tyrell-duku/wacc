package frontend

import org.scalatest.funsuite.AnyFunSuite
import frontend.Rules._
import frontend.LiterParser._

class FrontendParserIntRepTest extends AnyFunSuite {

  test("Successfully parses octal integer") {
    assert(intLiter.runParser("0o007").contains(IntLiter(7, (1, 1))))
  }

  test("Successfully parses octal integer with positive sign") {
    assert(intLiter.runParser("+0o007").contains(IntLiter(7, (1, 1))))
  }

  test("Successfully parses octal integer with negative sign") {
    assert(intLiter.runParser("-0o007").contains(IntLiter(-7, (1, 1))))
  }

  test("Successfully parses hexadecimal integer") {
    assert(intLiter.runParser("0x007").contains(IntLiter(7, (1, 1))))
  }

  test("Successfully parses hexadecimal integer with positive sign") {
    assert(intLiter.runParser("+0x007").contains(IntLiter(7, (1, 1))))
  }

  test("Successfully parses hexadecimal integer with negative sign") {
    assert(intLiter.runParser("-0x006").contains(IntLiter(-6, (1, 1))))
  }

  test("Successfully parses binary integer") {
    assert(intLiter.runParser("0b001").contains(IntLiter(1, (1, 1))))
  }

  test("Successfully parses binary integer with positive sign") {
    assert(intLiter.runParser("+0b11").contains(IntLiter(3, (1, 1))))
  }

  test("Successfully parses binary integer with negative sign") {
    assert(intLiter.runParser("-0b11").contains(IntLiter(-3, (1, 1))))
  }

  test("Successfully fails to parse overflow binary number") {
    assert(intLiter.runParser("0b10000000000000000000000000000000").isFailure)
  }

  test("Successfully fails to parse negative overflow binary number") {
    assert(intLiter.runParser("0b10000000000000000000000000000000").isFailure)
  }

  test("Successfully fails to parse overflow octal number") {
    assert(intLiter.runParser("0o20000000000").isFailure)
  }

  test("Successfully fails to parse overflow negative octal number") {
    assert(intLiter.runParser("-0o20000000001").isFailure)
  }

  test("Successfully fails to parse overflow hexadecimal number") {
    assert(intLiter.runParser("0x80000000").isFailure)
  }

  test("Successfully fails to parse overflow negative hexadecimal number") {
    assert(intLiter.runParser("-0x80000001").isFailure)
  }

}
