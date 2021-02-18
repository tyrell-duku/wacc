import parsley.Parsley
import parsley.Parsley._
import parsley.combinator.{manyN, option, sepBy1}
import parsley.expr.{InfixL, Ops, Prefix, precedence}
import Rules._
import Lexer._
import LiterParser._

object ExprParser {
  // <int-liter> | <bool-liter> | <char-liter> | <str-liter> | <pair-liter> |
  // <ident> | <array-elem> | <unary-oper><expr> | <expr> <binary-oper> <expr>
  // '(' <expr> ')'
  val expr: Parsley[Expr] = precedence[Expr](
    intLiter <|> boolLiteral <|> charLiteral <|> strLiteral <|> pairLiteral <|>
      arrayElem <\> identifier <|> lexer.parens(expr),
    // unary operators
    Ops[Expr](Prefix)(
      Not("!") ? "unary operator",
      notFollowedBy(intLiter) *> Negation("-") ? "unary operator",
      Len("len") ? "unary operator",
      Ord("ord") ? "unary operator",
      Chr("chr") ? "unary operator"
    ),
    // arithmetic binary operators
    Ops[Expr](InfixL)(
      Mul("*") ? "arithmetic operator",
      Div("/") ? "arithmetic operator",
      Mod("%") ? "arithmetic operator"
    ),
    // arithmetic binary operators
    Ops[Expr](InfixL)(
      Plus("+") ? "arithmetic operator",
      Sub("-") ? "arithmetic operator"
    ),
    // comparison binary operators
    Ops[Expr](InfixL)(
      (GTE(">=") ? "comparison operator") <\> (GT(">") ? "comparison operator"),
      (LTE("<=") ? "comparison operator") <\> (LT("<") ? "comparison operator")
    ),
    // comparison binary operators
    Ops[Expr](InfixL)(
      Equal("==") ? "comparison operator",
      NotEqual("!=") ? "comparison operator"
    ),
    // boolean binary operators
    Ops[Expr](InfixL)(And("&&") ? "boolean operator"),
    Ops[Expr](InfixL)(Or("||") ? "boolean operator")
  )

  // <expr> (',' <expr>)*
  val argList: Parsley[ArgList] =
    ArgList <#> sepBy1(expr, ",") ? "arg-list"

  // '[' (<expr> (',' <expr>)*)? ']'
  val arrayLiter: Parsley[ArrayLiter] = ArrayLiter(lexer.brackets(
    option(sepBy1(expr, ","))
  )) ? "array-liter"

  // <ident> ('['<expr>']')+
  lazy val arrayElem: Parsley[ArrayElem] = ArrayElem(identifier, manyN(1, lexer.brackets(expr))
  ) ? "array-elem"

  // "fst" <expr> | "snd" <expr>
  val pairElem: Parsley[PairElem] =
    (Fst("fst" *> expr) <|> Snd("snd" *> expr)) ? "pair-elem"
}
