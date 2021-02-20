package backend

import scala.collection.mutable.ListBuffer
import backend.Operand
import backend.Reg
import backend.Condition

package object InstructionSet {

  sealed trait Instruction

  // arithmetic
  case class Add(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = "ADD " + rd + ", " + rn + ", " + op2
  }
  case class Sub(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = "SUB " + rd + ", " + rn + ", " + op2
  }
  case class Mul(rd: Reg, rm: Reg, rs: Reg) extends Instruction {
    override def toString: String = "MUL " + rd + ", " + rm + ", " + rs
  }

  // comparison
  case class Cmp(rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = "CMP " + rn + ", " + op2
  }

  // logical
  case class And(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = "AND " + rd + ", " + rn + ", " + op2
  }
  case class Or(rd: Reg, rn: Reg, op2: Operand) extends Instruction {
    override def toString: String = "OR " + rd + ", " + rn + ", " + op2
  }

  case class Push(rs: List[Reg]) extends Instruction {
    override def toString: String = "PUSH " + "{" + rs.mkString(", ") + "}"
  }
  case class Pop(rs: List[Reg]) extends Instruction {
    override def toString: String = "POP " + "{" + rs.mkString(", ") + "}"
  }
  case class Ldr(rd: Reg, op2: Operand) extends Instruction {
    override def toString: String = "LDR " + rd + ", " + op2
  }

  // Branching
  case class Branch(label: String) extends Instruction {
    override def toString: String = "B " + label
  }
  case class BranchCond(cond: Condition, label: String) extends Instruction

  // Creating labels
  case class Define(label: String) extends Instruction
}
