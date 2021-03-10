package frontend

import org.scalatest.funsuite.AnyFunSuite
import frontend.Rules._
import frontend.LiterParser._


class ParserIntRepTest extends AnyFunSuite {

  test("Successfully parses octal integer") {
    assert(intLiter.runParser("0o007").contains(IntLiter(7, (1,1))))
  }

  test("Successfully parses octal integer with positive sign") {
    assert(intLiter.runParser("+0o007").contains(IntLiter(7, (1,1))))
  }

  test("Successfully parses octal integer with negative sign") {
    assert(intLiter.runParser("-0o007").contains(IntLiter(-7, (1,1))))
  }

  test("Successfully parses hexadecimal integer") {
    assert(intLiter.runParser("0x007").contains(IntLiter(7, (1,1))))
  }

  test("Successfully parses hexadecimal integer with positive sign") {
    assert(intLiter.runParser("+0x007").contains(IntLiter(7, (1,1))))
  }

  test("Successfully parses hexadecimal integer with negative sign") {
    assert(intLiter.runParser("-0x006").contains(IntLiter(-6, (1,1))))
  }

}
