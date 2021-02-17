import scala.collection.mutable.ListBuffer

object InstructionSet {
  sealed trait Intruction
  // arithmetic
  case class Add(rd: Register, rn: Register, op2: Operand) extends Intruction
  case class Sub(rd: Register, rn: Register, op2: Operand) extends Intruction
  case class Mul(rd: Register, rm: Register, rs: Register) extends Intruction
  // comparison
  case class Cmp(rn: Register, op2: Operand) extends Intruction
  // logical
  case class And(rd: Register, rn: Register, op2: Operand) extends Intruction
  case class Or(rd: Register, rn: Register, op2: Operand) extends Intruction

  case class Push(rs: ListBuffer[Register]) extends Intruction
  case class Pop(rs: ListBuffer[Register]) extends Intruction
  case class Ldr(rd: Register, op2: Operand) extends Intruction

  // Branching
  case class Branch(label: String)
  case class BranchCond(cond: Condition, label: String)

  case class Register(n: Int)

  sealed trait Operand
  sealed trait Condition

}
