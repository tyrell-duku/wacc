package backend.CodeGeneration

import backend.CodeGeneration.Arrays.loadArrayElem
import backend.CodeGenerator
import backend.CodeGenerator._
import backend.DefinedFuncs.PreDefinedFuncs._
import backend.IR.Condition.Condition
import backend.IR.InstructionSet._
import backend.IR.Operand._
import frontend.Rules._

import scala.collection.mutable.ListBuffer
import backend.IR

object Expressions {

  /* Translates unary operator OP to the internal representation. */
  private def transUnOp(op: UnOp, reg: Reg): ListBuffer[Instruction] = {
    op match {
      case Chr(e, _) => transExp(e, reg)
      case Len(id: Ident, _) =>
        val (index, _) = sTable(id)
        ListBuffer(
          Ldr(reg, RegisterOffset(SP, currentSP - index)),
          Ldr(reg, RegAdd(reg))
        )
      case Negation(e, _) =>
        transExp(e, reg) ++= ListBuffer(
          RsbS(reg, reg, ImmInt(0)),
          BranchLinkCond(IR.Condition.VS, addRuntimeError(Overflow))
        )
      case Not(e, _) => transExp(e, reg) += Eor(reg, reg, ImmInt(1))
      case Ord(e, _) => transExp(e, reg)
      case _         => ListBuffer.empty[Instruction]
    }
  }

  /* Changes a comparison operator to the Condition equivalent.
     Returns null when not a comparison operator. */
  private def rulesCmpToInstrCmp(cmp: BinOp): Condition = {
    cmp match {
      case _: GT       => backend.IR.Condition.GT
      case _: GTE      => backend.IR.Condition.GE
      case _: LT       => backend.IR.Condition.LT
      case _: LTE      => backend.IR.Condition.LE
      case _: Equal    => backend.IR.Condition.EQ
      case _: NotEqual => backend.IR.Condition.NE
      case _           => null // Undefined
    }
  }

  /* Translates a comparator operator to the internal representation. */
  private def transCond(
      op: BinOp,
      reg: Reg,
      rReg: Reg
  ): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    val cmp = rulesCmpToInstrCmp(op)
    instructions += Cmp(reg, rReg)
    instructions += MovCond(cmp, reg, ImmInt(1))
    instructions += MovCond(cmp.oppositeCmp, reg, ImmInt(0))
    instructions
  }

  /* Translates a binary operator to the internal representation. */
  private def transBinOp(op: BinOp, reg: Reg): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    val rReg = getFreeReg()
    instructions ++= transExp(op.lExpr, reg)
    instructions ++= transExp(op.rExpr, rReg)
    op match {
      case _: frontend.Rules.Mul =>
        // Runtime error check
        instructions += SMul(reg, rReg, reg, rReg)
        instructions += Cmp(rReg, ASR(reg, ImmInt(31)))
        instructions += BranchLinkCond(
          IR.Condition.NE,
          addRuntimeError(Overflow)
        )
      case _: Div =>
        // Values need to be in R0 and R1 for "__aeabi_idiv"
        instructions += Mov(R0, reg)
        instructions += Mov(R1, rReg)
        // Runtime error check
        instructions += BranchLink(addRuntimeError(DivideByZero))
        // Divide function
        instructions += BranchLink(Label("__aeabi_idiv"))
        instructions += Mov(reg, R0)
      case _: Mod =>
        // Needs to be in R0 and R1 for "__aeabi_idivmod"
        instructions += Mov(R0, reg)
        instructions += Mov(R1, rReg)
        // Runtime error check
        instructions += BranchLink(addRuntimeError(DivideByZero))
        // Mod function
        instructions += BranchLink(Label("__aeabi_idivmod"))
        instructions += Mov(reg, R1)
      case _: Plus =>
        instructions += AddS(reg, reg, rReg)
        // Runtime error check
        instructions += BranchLinkCond(
          IR.Condition.VS,
          addRuntimeError(Overflow)
        )
      case _: frontend.Rules.Sub =>
        instructions += SubS(reg, reg, rReg)
        // Runtime error check
        instructions += BranchLinkCond(
          IR.Condition.VS,
          addRuntimeError(Overflow)
        )
      case _: frontend.Rules.And =>
        instructions += IR.InstructionSet.And(reg, reg, rReg)
      case _: frontend.Rules.Or =>
        instructions += IR.InstructionSet.Or(reg, reg, rReg)
      // Comparison binary operators
      case cmpOp => instructions ++= transCond(cmpOp, reg, rReg)
    }
    addUnusedReg(rReg)
    instructions
  }

  private def transStrLiter(
      str: StrLiter,
      reg: Reg
  ): ListBuffer[Instruction] = {
    val curLabel = CodeGenerator.dataTable.addDataEntry(str)
    ListBuffer(Ldr(reg, DataLabel(curLabel)))
  }

  /* Translates an expression operator to the internal representation. */
  def transExp(e: Expr, reg: Reg): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    e match {
      case IntLiter(n, _)  => instructions += Ldr(reg, ImmMem(n))
      case BoolLiter(b, _) => instructions += Mov(reg, ImmInt(boolToInt(b)))
      case CharLiter(c, _) => instructions += Mov(reg, ImmChar(c))
      case str: StrLiter   => instructions ++= transStrLiter(str, reg)
      case PairLiter(_)    => instructions += Ldr(reg, ImmMem(0))
      case id: Ident =>
        val (index, t) = sTable(id)
        val spOffset = currentSP - index
        instructions += Ldr(isByte(t), reg, SP, spOffset)
      case ArrayElem(id, es, _) => instructions ++= loadArrayElem(id, es, reg)
      case e: UnOp              => instructions ++= transUnOp(e, reg)
      case e: BinOp             => instructions ++= transBinOp(e, reg)
    }
    instructions
  }

}
