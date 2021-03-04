package frontend

import parsley.Parsley
import parsley.Parsley._
import parsley.combinator.{between, eof, option, sepBy1, many}
import parsley.lift.{lift2, lift4}
import frontend.Rules._
import frontend.Lexer._
import frontend.LiterParser._
import frontend.StatParser._

object Parser {
  // <type> <ident>
  val param: Parsley[Param] = lift2(Param, types, identifier) ? "param"

  // <param> (, <param>)*
  val paramList: Parsley[ParamList] =
    ParamList <#> sepBy1(param, ",") ? "param-list"

  // Checks whether a function is ended with a return/exit statement
  private def statTerminates(stat: Stat): Boolean = stat match {
    case If(_, s1, s2)       => statTerminates(s1) && statTerminates(s2)
    case While(_, s)         => statTerminates(s)
    case Begin(s)            => statTerminates(s)
    case Seq(s)              => statTerminates(s.last)
    case Exit(_) | Return(_) => true
    case _                   => false
  }

  // Error message if function not ended with a return/exit statement
  private def funcMsg(f: Func): String = f match {
    case Func(_, Ident(s, _), _, _) =>
      "Function " + s + " is not ended with return or exit statement"
  }

  // <type> <ident> (<param-list>) "is"" <stat> "end"
  val func: Parsley[Func] = lift4(
    Func,
    types,
    identifier,
    lexer.parens(option(paramList)),
    between(
      "is",
      "end",
      stat
    ) ? "function"
  ).guard((x: Func) => statTerminates(x.s), (x: Func) => funcMsg(x))

  // "begin" <func>* <stat> "end"
  val program: Parsley[Program] =
    between(
      "begin".explain("every program must start with \"begin\""),
      "end".explain("every program must terminate with \"end\""),
      lift2(Program, many(attempt(func)), stat)
    ) ? "program"

  // WACC file parser
  val waccParser: Parsley[Program] = lexer.whiteSpace *> program <* eof
}
