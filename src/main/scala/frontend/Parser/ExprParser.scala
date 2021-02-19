import parsley.Parsley
import parsley.Parsley._
import parsley.combinator.{manyN, option, sepBy1}
import parsley.expr.{InfixL, Ops, Postfix, Prefix, precedence}
import Rules._
import Lexer._
import LiterParser._

object ExprParser {
  // <expr> (',' <expr>)*
  val argList: Parsley[ArgList] =
    ArgList <#> sepBy1(expr, ",") ? "<expr> (',' <expr>)*"

  // '[' (<expr> (',' <expr>)*)? ']'
  val arrayLiter: Parsley[ArrayLiter] = ArrayLiter <#> lexer.brackets(
    option(sepBy1(expr, ","))
  ) ? "[(<expr> (',' <expr>)*)?]"

  // <ident> ('['<expr>']')+
  lazy val arrayElem: Parsley[ArrayElem] = lift2(
    ArrayElem,
    identifier,
    manyN(1, lexer.brackets(expr))
  ) ? "<ident> ([<expr>])+"

  // "fst" <expr> | "snd" <expr>
  val pairElem: Parsley[PairElem] =
    (Fst <#> "fst" *> expr ? "fst <expr>") <|> (Snd <#> "snd" *> expr ? "snd <expr>")

  // <int-liter> | <bool-liter> | <char-liter> | <str-liter> | <pair-liter> |
  // <ident> | <array-elem> | <unary-oper><expr> | <expr> <binary-oper> <expr>
  // '(' <expr> ')'
  val expr: Parsley[Expr] = precedence[Expr](
    intLiter <|> boolLiteral <|> charLiteral <|> strLiteral <|> pairLiteral <|>
      arrayElem <\> identifier <|> (Parens <#> lexer.parens(expr)),
    // unary operators
    Ops[Expr](Prefix)(
      "!" #> Not ? "unary operator",
      notFollowedBy(intLiter) *> "-" #> Negation ? "unary operator",
      lexer.keyword("len") #> Len ? "unary operator",
      lexer.keyword("ord") #> Ord ? "unary operator",
      lexer.keyword("chr") #> Chr ? "unary operator"
    ),
    // arithmetic binary operators
    Ops[Expr](InfixL)(
      "*" #> Mul ? "artihmetic operator",
      "/" #> Div ? "artihmetic operator",
      "%" #> Mod ? "artihmetic operator"
    ),
    // arithmetic binary operators
    Ops[Expr](InfixL)(
      "+" #> Plus ? "artihmetic operator",
      "-" #> Sub ? "artihmetic operator"
    ),
    // comparison binary operators
    Ops[Expr](InfixL)(
      (">=" #> GTE ? "comparison operator") <\> (">" #> GT ? "comparison operator"),
      ("<=" #> LTE ? "comparison operator") <\> ("<" #> LT ? "comparison operator")
    ),
    // comparison binary operators
    Ops[Expr](InfixL)(
      "==" #> Equal ? "comparison operator",
      "!=" #> NotEqual ? "comparison operator"
    ),
    // boolean binary operators
    Ops[Expr](InfixL)("&&" #> And ? "boolean operator"),
    Ops[Expr](InfixL)("||" #> Or ? "boolean operator")
  )
}
