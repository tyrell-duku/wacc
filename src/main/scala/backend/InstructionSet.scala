package backend

import scala.collection.mutable.ListBuffer
import backend.Operand
import backend.Reg
import backend.Condition

package object InstructionSet {

  sealed trait Instruction

  // arithmetic
  case class Add(rd: Reg, rn: Reg, op2: Operand) extends Instruction
  case class Sub(rd: Reg, rn: Reg, op2: Operand) extends Instruction
  case class Mul(rd: Reg, rm: Reg, rs: Reg) extends Instruction

  // comparison
  case class Cmp(rn: Reg, op2: Operand) extends Instruction

  // logical
  case class And(rd: Reg, rn: Reg, op2: Operand) extends Instruction
  case class Or(rd: Reg, rn: Reg, op2: Operand) extends Instruction

  case class Push(rs: ListBuffer[Reg]) extends Instruction
  case class Pop(rs: ListBuffer[Reg]) extends Instruction
  case class Ldr(rd: Reg, op2: Operand) extends Instruction

  // Branching
  case class Branch(label: String) extends Instruction
  case class BranchCond(cond: Condition, label: String) extends Instruction

  // Creating labels
  case class Define(label: String) extends Instruction
}
