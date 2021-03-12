package backend.CodeGeneration

import backend.CodeGeneration.Arrays.loadArrayElem
import backend.CodeGenerator
import backend.CodeGenerator._
import backend.DefinedFuncs.PreDefinedFuncs.{Overflow, DivideByZero}
import backend.DefinedFuncs.RuntimeErrors.addRuntimeError
import backend.IR.Condition.Condition
import backend.IR.InstructionSet._
import backend.IR.Operand._
import frontend.Rules._
import scala.collection.mutable.ListBuffer
import backend.IR

object Expressions {
  /* Translates unary operator OP to the internal representation,
     into the internal . */
  private def transUnOp(op: UnOp, rd: Reg): ListBuffer[Instruction] = {
    op match {
      case Chr(e, _) => transExp(e, rd)
      case Len(id: Ident, _) =>
        val (index, _) = sTable(id)
        ListBuffer(
          Ldr(rd, RegisterOffset(SP, currentSP - index)),
          Ldr(rd, RegAdd(rd))
        )
      case Negation(e, _) =>
        transExp(e, rd) ++= ListBuffer(
          RsbS(rd, rd, ImmInt(0)),
          BranchLinkCond(IR.Condition.VS, addRuntimeError(Overflow))
        )
      case Not(e, _)        => transExp(e, rd) += Eor(rd, rd, ImmInt(TRUE_INT))
      case Ord(e, _)        => transExp(e, rd)
      case BitwiseNot(e, _) => transExp(e, rd) += MvN(rd, rd)
      case _                => ListBuffer.empty[Instruction]
    }
  }

  /* Changes a comparison operator to the Condition equivalent.
     Returns ??? when not a comparison operator. */
  private def rulesCmpToInstrCmp(cmp: BinOp): Condition = {
    cmp match {
      case _: GT       => backend.IR.Condition.GT
      case _: GTE      => backend.IR.Condition.GE
      case _: LT       => backend.IR.Condition.LT
      case _: LTE      => backend.IR.Condition.LE
      case _: Equal    => backend.IR.Condition.EQ
      case _: NotEqual => backend.IR.Condition.NE
      case _           => ??? // Undefined
    }
  }

  /* Translates a comparator operator to the internal representation. */
  private def transCond(
      op: BinOp,
      rd: Reg,
      rm: Reg
  ): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    val cmp = rulesCmpToInstrCmp(op)
    instructions += Cmp(rd, rm)
    instructions += MovCond(cmp, rd, ImmInt(TRUE_INT))
    instructions += MovCond(cmp.oppositeCmp, rd, ImmInt(FALSE_INT))
    instructions
  }

  /* Translates a binary operator to the internal representation. */
  private def transBinOp(op: BinOp, rd: Reg): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    instructions ++= transExp(op.lExpr, rd)
    val rm = getFreeReg()
    instructions ++= transExp(op.rExpr, rm)
    // Register over allocation check, if true pops variable from stack into popReg
    var newReg = rd
    if (popReg == rm) {
      instructions += Pop(ListBuffer(popReg))
      newReg = R10
    }

    op match {
      case _: frontend.Rules.Mul =>
        // Runtime error check
        instructions += SMul(newReg, rm, newReg, rm)
        instructions += Cmp(rm, ASR(newReg, ImmInt(31)))
        instructions += BranchLinkCond(
          IR.Condition.NE,
          addRuntimeError(Overflow)
        )
      case _: Div =>
        // Values need to be in R0 and R1 for "__aeabi_idiv"
        instructions += Mov(resultReg, newReg)
        instructions += Mov(R1, rm)
        // Runtime error check
        instructions += BranchLink(addRuntimeError(DivideByZero))
        // Divide function
        instructions += BranchLink(Label("__aeabi_idiv"))
        instructions += Mov(newReg, resultReg)
      case _: Mod =>
        // Needs to be in R0 and R1 for "__aeabi_idivmod"
        instructions += Mov(resultReg, newReg)
        instructions += Mov(R1, rm)
        // Runtime error check
        instructions += BranchLink(addRuntimeError(DivideByZero))
        // Mod function
        instructions += BranchLink(Label("__aeabi_idivmod"))
        instructions += Mov(newReg, R1)
      case _: Plus =>
        instructions += AddS(newReg, newReg, rm)
        // Runtime error check
        instructions += BranchLinkCond(
          IR.Condition.VS,
          addRuntimeError(Overflow)
        )
      case _: frontend.Rules.Sub =>
        instructions += SubS(newReg, newReg, rm)
        // Runtime error check
        instructions += BranchLinkCond(
          IR.Condition.VS,
          addRuntimeError(Overflow)
        )
      case _: frontend.Rules.And =>
        instructions += IR.InstructionSet.And(newReg, newReg, rm)
      case _: frontend.Rules.Or =>
        instructions += IR.InstructionSet.Or(newReg, newReg, rm)
      // Comparison binary operators
      case cmpOp => instructions ++= transCond(cmpOp, newReg, rm)
    }
    addUnusedReg(rm)
    instructions
  }

  /* Pushes variable in R10 onto stack if registers are over allocated */
  private def regAccumulate(r: Reg): (Reg, ListBuffer[Instruction]) = {
    val instructions = ListBuffer.empty[Instruction]
    var overflowReg = r
    if (r == popReg) {
      instructions += Push(ListBuffer(R10))
      overflowReg = R10
    }
    (overflowReg, instructions)
  }

  private def transStrLiter(
      str: StrLiter,
      rd: Reg
  ): ListBuffer[Instruction] = {
    val curLabel = CodeGenerator.dataTable.addDataEntry(str)
    ListBuffer(Ldr(rd, DataLabel(curLabel)))
  }

  /* Translates an expression operator to the internal representation. */
  def transExp(e: Expr, rd: Reg): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    e match {
      case IntLiter(n, _) =>
        val (newReg, instrs) = regAccumulate(rd)
        instructions ++= instrs
        instructions += Ldr(newReg, ImmMem(n))
      case BoolLiter(b, _) =>
        val (newReg, instrs) = regAccumulate(rd)
        instructions ++= instrs
        instructions += Mov(rd, ImmInt(boolToInt(b)))
      case CharLiter(c, _) => instructions += Mov(rd, ImmChar(c))
      case str: StrLiter   => instructions ++= transStrLiter(str, rd)
      case PairLiter(_)    => instructions += Ldr(rd, ImmMem(0))
      case id: Ident =>
        val (index, t) = sTable(id)
        val spOffset = currentSP - index
        instructions += Ldr(isByte(t), rd, SP, spOffset)
      case ArrayElem(id, es, _) => instructions ++= loadArrayElem(id, es, rd)
      case e: UnOp              => instructions ++= transUnOp(e, rd)
      case e: BinOp =>
        instructions ++= transBinOp(e, rd)
    }
    instructions
  }

}
