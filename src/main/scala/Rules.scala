object Rules {

  sealed case class Program(fs: List[Func], s: Stat)

  sealed case class Func(
      t: Type,
      i: Ident,
      ps: Option[ParamList] = None,
      s: Stat
  )

  sealed case class ParamList(ps: List[Param])

  sealed case class Param(t: Type, i: Ident)

  sealed trait Stat
  case object Skip extends Stat
  case class EqIdent(t: Type, i: Ident, a: AssignRHS) extends Stat
  case class EqAssign(l: AssignLHS, r: AssignRHS) extends Stat
  case class Read(x: AssignLHS) extends Stat
  case class Free(x: Expr) extends Stat
  case class Return(x: Expr) extends Stat
  case class Exit(x: Expr) extends Stat
  case class Print(x: Expr) extends Stat
  case class PrintLn(x: Expr) extends Stat
  case class If(x: Expr, y: Stat, z: Stat) extends Stat
  case class While(x: Expr, y: Stat) extends Stat
  case class Begin(x: Stat) extends Stat
  case class Seq(x: Stat, y: Stat) extends Stat

  sealed trait AssignLHS

  sealed trait AssignRHS
  case class Newpair(x: Expr, y: Expr) extends AssignRHS
  case class Call(x: Ident, y: Option[ArgList] = None) extends AssignRHS

  sealed case class ArgList(x: List[Expr])

  sealed trait PairElem extends AssignLHS with AssignRHS
  case class Fst(x: Expr) extends PairElem
  case class Snd(x: Expr) extends PairElem

  sealed trait Type

  sealed trait BaseType extends Type
  case object IntT extends BaseType
  case object BoolT extends BaseType
  case object CharT extends BaseType
  case object StringT extends BaseType

  sealed trait ArrayType extends Type
  case class OfArrayType(x: Type) extends ArrayType

  sealed trait PairType extends Type
  case class Pair(x: PairElemType, y: PairElemType) extends PairType

  sealed trait PairElemType
  case object PairElemPair extends PairElemType
  case class PairElemT(x: Type) extends PairElemType

  sealed trait Expr extends AssignRHS
  case class Parens(x: Expr) extends Expr

  sealed trait UnOp extends Expr
  case class Not(x: Expr) extends UnOp
  case class Negation(x: Expr) extends UnOp
  case class Len(x: Expr) extends UnOp
  case class Ord(x: Expr) extends UnOp
  case class Chr(x: Expr) extends UnOp

  sealed trait BinOp extends Expr
  case class Mul(l: Expr, r: Expr) extends BinOp
  case class Div(l: Expr, r: Expr) extends BinOp
  case class Mod(l: Expr, r: Expr) extends BinOp
  case class Plus(l: Expr, r: Expr) extends BinOp
  case class Sub(l: Expr, r: Expr) extends BinOp
  case class GT(l: Expr, r: Expr) extends BinOp
  case class GTE(l: Expr, r: Expr) extends BinOp
  case class LT(l: Expr, r: Expr) extends BinOp
  case class LTE(l: Expr, r: Expr) extends BinOp
  case class Equal(l: Expr, r: Expr) extends BinOp
  case class NotEqual(l: Expr, r: Expr) extends BinOp
  case class And(l: Expr, r: Expr) extends BinOp
  case class Or(l: Expr, r: Expr) extends BinOp

  sealed case class Ident(x: String) extends AssignLHS with AssignRHS with Expr

  sealed case class ArrayElem(x: Ident, y: List[Expr])
      extends AssignLHS
      with Expr

  sealed case class IntLiter(x: Int) extends Expr

  sealed case class BoolLiter(x: Boolean) extends Expr

  sealed case class CharLiter(x: Character) extends Expr

  sealed trait Character
  case class NormalChar(x: Char) extends Character
  case class Escape(x: Char) extends Character

  sealed case class StrLiter(x: List[Character]) extends Expr

  sealed case class ArrayLiter(x: Option[List[Expr]]) extends AssignRHS

  sealed case class PairLiter() extends Expr

}
