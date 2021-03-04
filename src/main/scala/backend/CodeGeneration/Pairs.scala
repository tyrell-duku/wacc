package backend.CodeGeneration

import backend.CodeGeneration.Assignments.assignRHS
import backend.CodeGeneration.Expressions.transExp
import backend.CodeGenerator._
import backend.DefinedFuncs.PreDefinedFuncs.{NullPointer, addRuntimeError}
import backend.IR.InstructionSet._
import backend.IR.Operand._
import frontend.Rules._

import scala.collection.mutable.ListBuffer

object Pairs {

  private def pairIsByte(idType: Type, fst: Boolean): Boolean = {
    idType match {
      case Pair(PairElemT(x), PairElemT(y)) => if (fst) isByte(x) else isByte(y)
      case _                                => false
    }
  }

  def transPairElem(
      id: Ident,
      fst: Boolean,
      freeReg: Reg
  ): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    // Loads in pair from ident into freeReg
    instructions ++= transExp(id, freeReg)
    // Value must be in R0 for branch
    instructions += Mov(R0, freeReg)
    // Runtime error
    instructions += BranchLink(addRuntimeError(NullPointer))

    if (fst) {
      // For Fst
      instructions += Ldr(freeReg, RegAdd(freeReg))
    } else {
      // For Snd
      instructions += Ldr(freeReg, RegisterOffset(freeReg, PAIR_SIZE))
    }
    instructions
  }

  def loadPairElem(
      id: Ident,
      freeReg: Reg,
      isFst: Boolean
  ): Instruction = {
    val (_, idType) = sTable(id)
    if (pairIsByte(idType, isFst)) {
      LdrSB(freeReg, RegAdd(freeReg))
    } else {
      Ldr(freeReg, RegAdd(freeReg))
    }
  }

  def assignRHSPair(
      p: Type,
      fst: Expr,
      snd: Expr,
      freeReg: Reg
  ): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]

    val Pair(fstType, sndType) = p
    val nextFreeReg = getFreeReg()
    // Every pair requires 8 bytes
    instructions += Ldr(R0, ImmMem(2 * PAIR_SIZE))
    instructions += BranchLink(Label("malloc"))
    instructions += Mov(freeReg, R0)
    instructions ++= transExp(fst, nextFreeReg)
    // Size of fst rhs
    instructions += Ldr(R0, ImmMem(getPairElemTypeSize(fstType)))
    instructions += BranchLink(Label("malloc"))
    if (pairIsByte(p, true)) {
      instructions += StrB(nextFreeReg, RegAdd(R0))
    } else {
      instructions += Str(nextFreeReg, RegAdd(R0))
    }
    instructions += Str(R0, RegAdd(freeReg))
    instructions ++= transExp(snd, nextFreeReg)
    // Size of snd rhs
    instructions += Ldr(R0, ImmMem(getPairElemTypeSize(sndType)))
    instructions += BranchLink(Label("malloc"))
    if (pairIsByte(p, false)) {
      instructions += StrB(nextFreeReg, RegAdd(R0))
    } else {
      instructions += Str(nextFreeReg, RegAdd(R0))
    }
    addUnusedReg(nextFreeReg)
    instructions += Str(R0, RegisterOffset(freeReg, PAIR_SIZE))

    instructions
  }

  def transEqAssignPairElem(
      rhs: AssignRHS,
      id: Ident,
      isFst: Boolean,
      freeReg: Reg
  ): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    val (index, t) = sTable(id)
    val pairElemType = if (isFst) {
      t match {
        case Pair(PairElemT(fstType), _) => fstType
        case Pair(PairElemPair, _)       => Pair(null, null)
        // Semantically incorrect
        case _ => null
      }
    } else {
      t match {
        case Pair(_, PairElemT(sndType)) => sndType
        case Pair(_, PairElemPair)       => Pair(null, null)
        // Semantically incorrect
        case _ => null
      }
    }
    val (isByte, instrs) = assignRHS(pairElemType, rhs, freeReg)
    instructions ++= instrs
    val nextReg = getFreeReg()
    instructions ++= transPairElem(id, isFst, nextReg)
    if (isByte) {
      instructions += StrB(freeReg, RegAdd(nextReg))
    } else {
      instructions += Str(freeReg, RegAdd(nextReg))
    }
    addUnusedReg(nextReg)
    instructions
  }

  private def getPairElemTypeSize(pairType: PairElemType): Int = {
    pairType match {
      case PairElemPair        => PAIR_SIZE
      case PairElemT(baseType) => getBaseTypeSize(baseType)
    }
  }
}
