import scala.collection.mutable.ListBuffer
import backend.Operand
import backend.Reg
import backend.Condition

object InstructionSet {
  sealed trait Intruction
  // arithmetic
  case class Add(rd: Reg, rn: Reg, op2: Operand) extends Intruction
  case class Sub(rd: Reg, rn: Reg, op2: Operand) extends Intruction
  case class Mul(rd: Reg, rm: Reg, rs: Reg) extends Intruction
  // comparison
  case class Cmp(rn: Reg, op2: Operand) extends Intruction
  // logical
  case class And(rd: Reg, rn: Reg, op2: Operand) extends Intruction
  case class Or(rd: Reg, rn: Reg, op2: Operand) extends Intruction

  case class Push(rs: ListBuffer[Reg]) extends Intruction
  case class Pop(rs: ListBuffer[Reg]) extends Intruction
  case class Ldr(rd: Reg, op2: Operand) extends Intruction

  // Branching
  case class Branch(label: String)
  case class BranchCond(cond: Condition, label: String)
}
