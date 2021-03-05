package backend.CodeGeneration

import backend.CodeGeneration.Assignments.assignRHS
import backend.CodeGeneration.Expressions.transExp
import backend.CodeGenerator._
import backend.DefinedFuncs.PreDefinedFuncs.NullPointer
import backend.IR.InstructionSet._
import backend.IR.Operand._
import frontend.Rules._
import backend.DefinedFuncs.RuntimeErrors.addRuntimeError
import scala.collection.mutable.ListBuffer

object Pairs {
  /* Returns whether the type IDTYPE has the size of byte. */
  private def pairIsByte(idType: Type, isFst: Boolean): Boolean = {
    idType match {
      case Pair(PairElemT(x), PairElemT(y)) =>
        if (isFst) isByte(x) else isByte(y)
      case _ => false
    }
  }

  /* Translates pair-elems to the internal representation and loads
     the result into the register RD.*/
  def transPairElem(
      id: Ident,
      fst: Boolean,
      rd: Reg
  ): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    // Loads in pair from ident into rd
    instructions ++= transExp(id, rd)
    // Value must be in resultReg for branch
    instructions += Mov(resultReg, rd)
    // Runtime error
    instructions += BranchLink(addRuntimeError(NullPointer))
    if (fst) {
      // For Fst
      instructions += Ldr(rd, RegAdd(rd))
    } else {
      // For Snd
      instructions += Ldr(rd, rd, PAIR_SIZE)
    }
    instructions
  }
  /* Loads an element with identifier ID into register RD. Loads the first
     element of pair ID into RD if ISFST is true, otherwise it laods the second
     element. */
  def loadPairElem(
      id: Ident,
      rd: Reg,
      isFst: Boolean
  ): Instruction = {
    val (_, idType) = sTable(id)
    Ldr(pairIsByte(idType, isFst), rd, rd, NO_OFFSET)
  }

  /* Translates a pair P with first element FST and second element SND into the
     internal representation, loading P into register RD. */
  def assignRHSPair(
      p: Type,
      fst: Expr,
      snd: Expr,
      rd: Reg
  ): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    val Pair(fstType, sndType) = p
    val nextFreeReg = getFreeReg()
    // Every pair requires 8 bytes
    instructions += Ldr(resultReg, ImmMem(2 * PAIR_SIZE))
    instructions += BranchLink(Label("malloc"))
    instructions += Mov(rd, resultReg)
    instructions ++= transExp(fst, nextFreeReg)
    // Size of fst rhs
    instructions += Ldr(resultReg, ImmMem(getPairElemTypeSize(fstType)))
    instructions += BranchLink(Label("malloc"))
    instructions += Str(
      pairIsByte(p, IS_FST_ELEM),
      nextFreeReg,
      resultReg,
      NO_OFFSET
    )
    instructions += Str(resultReg, RegAdd(rd))
    instructions ++= transExp(snd, nextFreeReg)
    // Size of snd rhs
    instructions += Ldr(resultReg, ImmMem(getPairElemTypeSize(sndType)))
    instructions += BranchLink(Label("malloc"))
    instructions += Str(
      pairIsByte(p, IS_SND_ELEM),
      nextFreeReg,
      resultReg,
      NO_OFFSET
    )
    addUnusedReg(nextFreeReg)
    instructions += Str(resultReg, rd, PAIR_SIZE)
    instructions
  }

  /* Translates the assignment of a pair-element into the internal
     representation, loading the element into register RD.
     Loads the first element if ISFST is true, and the second otherwise.  */
  def transEqAssignPairElem(
      rhs: AssignRHS,
      id: Ident,
      isFst: Boolean,
      rd: Reg
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
    val (isByte, instrs) = assignRHS(pairElemType, rhs, rd)
    instructions ++= instrs
    val nextReg = getFreeReg()
    instructions ++= transPairElem(id, isFst, nextReg)
    instructions += Str(isByte, rd, nextReg, NO_OFFSET)
    addUnusedReg(nextReg)
    instructions
  }

  /* Returns the size of the pair-elem type PAIRTYPE in bytes, as an int. */
  private def getPairElemTypeSize(pairType: PairElemType): Int = {
    pairType match {
      case PairElemPair        => PAIR_SIZE
      case PairElemT(baseType) => getBaseTypeSize(baseType)
    }
  }
}
