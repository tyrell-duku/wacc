import parsley.Parsley._
import parsley.implicits.{stringLift, charLift}
import parsley.{Parsley, Result}
import parsley.character.{anyChar}

object Rules {

  sealed abstract class Program(fs: Array[Func], s: Stat)

  sealed abstract class Func(
      t: Type,
      i: Ident,
      ps: Option[ParamList] = None,
      s: Stat
  )

  sealed abstract class ParamList(ps: Array[Param])

  sealed abstract class Param(t: Type, i: Ident)

  sealed trait Stat
  case object Skip extends Stat
  case class EqIdent(t: Type, i: Ident, a: AssignLHS) extends Stat
  case class EqAssign(l: AssignRHS, r: AssignLHS) extends Stat
  case class Read(x: AssignLHS) extends Stat
  case class Free(x: Expr) extends Stat
  case class Return(x: Expr) extends Stat
  case class Exit(x: Expr) extends Stat
  case class Print(x: Expr) extends Stat
  case class PrintLn(x: Expr) extends Stat
  case class If(x: Expr, y: Stat, z: Stat) extends Stat
  case class While(x: Expr, y: Stat) extends Stat
  case class Begin(x: Stat) extends Stat
  case class Colon(x: Stat, y: Stat) extends Stat

  sealed trait AssignLHS

  sealed trait AssignRHS
  case class Newpair(x: Expr, y: Expr) extends AssignRHS
  case class Call(x: Ident, y: Option[ArgList] = None) extends AssignRHS

  sealed abstract class ArgList(x: Array[Expr])

  sealed trait PairElem extends AssignLHS with AssignRHS
  case class Fst(x: Expr) extends PairElem
  case class Snd(x: Expr) extends PairElem

  sealed trait Type

  sealed trait BaseType extends Type with PairElemType
  case object IntT extends BaseType
  case object BoolT extends BaseType
  case object CharT extends BaseType
  case object StringT extends BaseType

  sealed trait ArrayType extends Type with PairElemType
  case class OfArrayType(x: Type) extends ArrayType

  sealed trait PairType extends Type
  case class Pair(x: PairElemType, y: PairElemType) extends PairType

  sealed trait PairElemType
  case object PairElemPair extends PairElemType

  sealed trait Expr extends AssignRHS
  case class Unary(x: UnOp, y: Expr) extends Expr
  case class Binary(l: Expr, op: BinOp, r: Expr) extends Expr
  case class Parens(x: Expr) extends Expr

  sealed trait UnOp
  case object Not extends UnOp
  case object Negation extends UnOp
  case object Len extends UnOp
  case object Ord extends UnOp
  case object Chr extends UnOp

  sealed trait BinOp
  case object Mul extends BinOp
  case object Div extends BinOp
  case object Mod extends BinOp
  case object Plus extends BinOp
  case object Sub extends BinOp
  case object GT extends BinOp
  case object GTE extends BinOp
  case object LT extends BinOp
  case object LTE extends BinOp
  case object Equal extends BinOp
  case object NotEqual extends BinOp
  case object And extends BinOp
  case object Or extends BinOp

  sealed abstract class Ident(x: String)
      extends AssignLHS
      with AssignRHS
      with Expr

  sealed abstract class ArrayElem(x: Ident, y: Array[Expr])
      extends AssignLHS
      with Expr

  sealed abstract class IntLiter(x: Option[IntSign] = None, y: Int) extends Expr

  sealed trait IntSign
  case object Pos extends IntSign
  case object Neg extends IntSign

  sealed abstract class BoolLiter(x: Boolean) extends Expr

  sealed abstract class CharLiter(x: Char) extends Expr

  sealed abstract class StrLiter(x: String) extends Expr

  sealed abstract class ArrayLiter(x: Array[Expr]) extends AssignRHS

  sealed trait PairLiter extends Expr

}
