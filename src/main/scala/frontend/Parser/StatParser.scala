import parsley.Parsley
import parsley.Parsley._
import parsley.combinator.{between, option, sepBy1}
import parsley.lift.{lift2, lift3}
import frontend.Rules._
import Lexer._
import LiterParser._
import ExprParser._

object StatParser {

  // <ident> | <array-elem> | <pair-elem>
  val assignLHS: Parsley[AssignLHS] = pairElem <|> arrayElem <\> identifier

  // <expr> | <array-liter> | newpair(<expr>, <expr>) | <pair-elem> | call <ident> (<arg-list>?)
  val assignRHS: Parsley[AssignRHS] =
    ("newpair" *> lexer
      .parens(
        Newpair(expr, "," *> expr)
      )) <|>
      (
        Call(
          "call" *>
            identifier,
          lexer.parens(option(argList))
        )
      ) <|> pairElem <|> expr <|> arrayLiter

  // "skip"
  private val skipStat: Parsley[Stat] = "skip" #> Skip

  // <type> <ident> = <assign-rhs>
  private val eqIdent: Parsley[Stat] =
    lift3(
      EqIdent,
      types,
      identifier,
      "=" *> assignRHS
    ).explain("identifier declaration")

  // <assign-lhs> = <assign-rhs>
  private val eqAssign: Parsley[Stat] =
    lift2(EqAssign, assignLHS, "=" *> assignRHS).explain("equal assignment")

  // <read> <assign=lhs>
  private val readStat: Parsley[Stat] =
    (Read <#> "read" *> assignLHS).explain("read")

  // "free" <expr>
  private val freeStat: Parsley[Stat] =
    (Free <#> "free" *> expr).explain("free")

  // "return" <expr>
  private val retStat: Parsley[Stat] =
    (Return <#> "return" *> expr).explain("return")

  // "exit" <expr>
  private val exitStat: Parsley[Stat] =
    (Exit <#> "exit" *> expr).explain("exit")

  // "print" <expr>
  private val printStat: Parsley[Stat] =
    (Print <#> "print" *> expr).explain("print")

  // "println" <expr>
  private val printlnStat: Parsley[Stat] =
    (PrintLn <#> "println" *> expr).explain("println")

  // if <expr> "then" <stat> "else" <stat> "fi"
  private val ifStat: Parsley[Stat] =
    ("if" *> lift3(
      If,
      expr,
      "then" *> stat,
      between("else", "fi", stat)
    )).explain("if")

  // "while" <expr> "do" <stat> "done"
  private val whileStat: Parsley[Stat] =
    ("while" *> lift2(
      While,
      expr,
      between("do", "done", stat)
    )).explain("while")

  // "begin" <stat> "end"
  private val beginStat: Parsley[Stat] =
    between("begin", "end", Begin <#> stat).explain("begin")

  private lazy val statement: Parsley[Stat] = skipStat <|> readStat <\>
    retStat <|> freeStat <|> exitStat <|> printlnStat <\> printStat <|>
    ifStat <|> whileStat <|> beginStat <|> eqIdent <|> eqAssign

  // statement | statement ; statement
  lazy val stat: Parsley[Stat] =
    (statement <* notFollowedBy(";")) ? "statement" <\> (Seq <#> sepBy1(
      statement ? "sequential statement",
      ";"
    )) //? "sequential statement")
}
