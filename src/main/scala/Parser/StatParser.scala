import parsley.Parsley
import parsley.Parsley._
import parsley.combinator.{between, option, sepBy1}
import Rules._
import Lexer._
import LiterParser._
import ExprParser._

object StatParser {

  // <ident> | <arrary-elem> | <pair-elem>
  val assignLHS: Parsley[AssignLHS] = pairElem <|> arrayElem <\> identifier

  // <expr> | <array-liter> | newpair(<expr>, <expr>) | <pair-elem> | call <ident> (<arg-list>?)
  val assignRHS: Parsley[AssignRHS] =
    ("newpair" *> lexer
      .parens(
        lift2(Newpair, expr, "," *> expr)
      )) <|>
      ("call" *> lift2(
        Call,
        identifier,
        lexer.parens(option(argList))
      )) <|> pairElem <|> expr <|> arrayLiter

  // "skip"
  private val skipStat: Parsley[Stat] = "skip" #> Skip

  // <type> <ident> = <assign-rhs>
  private val eqIdent: Parsley[Stat] =
    lift3(
      EqIdent,
      types,
      identifier,
      "=" *> assignRHS
    ) ? "<type> <ident> = <assign-rhs>"

  // <assign-lhs> = <assign-rhs>
  private val eqAssign: Parsley[Stat] =
    lift2(EqAssign, assignLHS, "=" *> assignRHS) ? "<assign-lhs> = <assign-rhs>"

  // <read> <assign=lhs>
  private val readStat: Parsley[Stat] =
    Read <#> "read" *> assignLHS ? "read <assign-lhs>"

  // "free" <expr>
  private val freeStat: Parsley[Stat] = Free <#> "free" *> expr ? "free <expr>"

  // "return" <expr>
  private val retStat: Parsley[Stat] =
    Return <#> "return" *> expr ? "return <expr>"

  // "exit" <expr>
  private val exitStat: Parsley[Stat] = Exit <#> "exit" *> expr ? "exit <expr>"

  // "print" <expr>
  private val printStat: Parsley[Stat] =
    Print <#> "print" *> expr ? "print <expr>"

  // "println" <expr>
  private val printlnStat: Parsley[Stat] =
    PrintLn <#> "println" *> expr ? "println <expr>"

  // if <expr> "then" <stat> "else" <stat> "fi"
  private val ifStat: Parsley[Stat] =
    "if" *> lift3(
      If,
      expr,
      "then" *> stat,
      between("else", "fi", stat)
    ) ? "if <expr> then <stat> else <stat> fi"

  // "while" <expr> "do" <stat> "done"
  private val whileStat: Parsley[Stat] =
    "while" *> lift2(
      While,
      expr,
      between("do", "done", stat)
    ) ? "while <expr> do <stat> done"

  // "begin" <stat> "end"
  private val beginStat: Parsley[Stat] =
    between("begin", "end", Begin <#> stat) ? "begin <stat> end"

  private lazy val statement: Parsley[Stat] = skipStat <|> readStat <\>
    retStat <|> freeStat <|> exitStat <|> printlnStat <\> printStat <|>
    ifStat <|> whileStat <|> beginStat <|> eqIdent <|> eqAssign

  // statement | statement ; statement
  lazy val stat: Parsley[Stat] =
    (statement <* notFollowedBy(";")) <\> (Seq <#> sepBy1(statement, ";"))
}
