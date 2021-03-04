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
          BranchLinkVS(addRuntimeError(Overflow))
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
      case GT(_, _, _)       => backend.IR.Condition.GT
      case GTE(_, _, _)      => backend.IR.Condition.GE
      case LT(_, _, _)       => backend.IR.Condition.LT
      case LTE(_, _, _)      => backend.IR.Condition.LE
      case Equal(_, _, _)    => backend.IR.Condition.EQ
      case NotEqual(_, _, _) => backend.IR.Condition.NE
      case _                 => null // Undefined
    }
  }

  /* Translates a comparator operator to the internal representation. */
  private def transCond(op: BinOp, reg: Reg): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]

    val rReg = getFreeReg()
    instructions ++= transExp(op.lExpr, reg)
    instructions ++= transExp(op.rExpr, rReg)
    val cmp = rulesCmpToInstrCmp(op)
    instructions += Cmp(reg, rReg)
    addUnusedReg(rReg)
    instructions += MovCond(cmp, reg, ImmInt(1))
    instructions += MovCond(cmp.oppositeCmp, reg, ImmInt(0))
    instructions
  }

  /* Translates a binary operator to the internal representation. */
  private def transBinOp(op: BinOp, reg: Reg): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    // TODO: determine result register for l & r
    op match {
      case frontend.Rules.Mul(l, r, _) =>
        val rReg = getFreeReg()
        instructions ++= transExp(l, reg)
        instructions ++= transExp(r, rReg)
        // Runtime error check
        instructions += SMul(reg, rReg, reg, rReg)
        instructions += Cmp(rReg, ASR(reg, ImmInt(31)))
        instructions += BranchLinkNE(addRuntimeError(Overflow))

        addUnusedReg(rReg)
      case Div(l, r, _) =>
        val rReg = getFreeReg()
        instructions ++= transExp(l, reg)
        instructions ++= transExp(r, rReg)
        // Needs to be in R0 and R1 for "__aeabi_idiv"
        instructions += Mov(R0, reg)
        instructions += Mov(R1, rReg)
        // Runtime error check
        instructions += BranchLink(addRuntimeError(DivideByZero))

        // Divide function
        instructions += BranchLink(Label("__aeabi_idiv"))
        addUnusedReg(rReg)
        instructions += Mov(reg, R0)
      case Mod(l, r, _) =>
        val rReg = getFreeReg()
        instructions ++= transExp(l, reg)
        instructions ++= transExp(r, rReg)

        // Needs to be in R0 and R1 for "__aeabi_idivmod"
        instructions += Mov(R0, reg)
        instructions += Mov(R1, rReg)
        // Runtime error check
        instructions += BranchLink(addRuntimeError(DivideByZero))

        // Mod function
        instructions += BranchLink(Label("__aeabi_idivmod"))
        addUnusedReg(rReg)
        instructions += Mov(reg, R1)
      case Plus(l, r, _) =>
        val rReg = getFreeReg()
        instructions ++= transExp(l, reg)
        instructions ++= transExp(r, rReg)
        instructions += AddS(reg, reg, rReg)
        // Runtime error check
        instructions += BranchLinkVS(addRuntimeError(Overflow))
        addUnusedReg(rReg)
      case frontend.Rules.Sub(l, r, _) =>
        val rReg = getFreeReg()
        instructions ++= transExp(l, reg)
        instructions ++= transExp(r, rReg)
        instructions += SubS(reg, reg, rReg)
        // Runtime error check
        instructions += BranchLinkVS(addRuntimeError(Overflow))
        addUnusedReg(rReg)
      case frontend.Rules.And(l, r, _) =>
        val rReg = getFreeReg()
        instructions ++= transExp(l, reg)
        instructions ++= transExp(r, rReg)
        instructions += backend.IR.InstructionSet.And(reg, reg, rReg)
        addUnusedReg(rReg)
      case frontend.Rules.Or(l, r, _) =>
        val rReg = getFreeReg()
        instructions ++= transExp(l, reg)
        instructions ++= transExp(r, rReg)
        instructions += backend.IR.InstructionSet.Or(reg, reg, rReg)
        addUnusedReg(rReg)
      // Comparison binary operators
      case cmpOp => instructions ++= transCond(cmpOp, reg)
    }
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
    // TODO: Determine free registers
    e match {
      case IntLiter(n, _)  => instructions += Ldr(reg, ImmMem(n))
      case BoolLiter(b, _) => instructions += Mov(reg, ImmInt(boolToInt(b)))
      // TODO: escaped character
      case CharLiter(c, _) => instructions += Mov(reg, ImmChar(c))
      case str: StrLiter   => instructions ++= transStrLiter(str, reg)
      case PairLiter(_)    => instructions += Ldr(reg, ImmMem(0))
      // TODO: track variable location
      case id: Ident =>
        val (index, t) = sTable(id)
        val spOffset = currentSP - index
        t match {
          case CharT | BoolT =>
            instructions += LdrSB(reg, RegisterOffset(SP, spOffset))
          case _ => instructions += Ldr(reg, RegisterOffset(SP, spOffset))
        }
      case ArrayElem(id, es, _) => instructions ++= loadArrayElem(id, es, reg)
      case e: UnOp              => instructions ++= transUnOp(e, reg)
      case e: BinOp             => instructions ++= transBinOp(e, reg)
    }
    instructions
  }

}
