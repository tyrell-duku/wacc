package backend.CodeGeneration

import backend.CodeGeneration.Arrays.transArrayElem
import backend.CodeGeneration.Pairs.transPairElem
import backend.CodeGenerator._
import backend.DefinedFuncs.ReadInstructions.{charRead, intRead}
import backend.DefinedFuncs.PreDefinedFuncs.{ReadInt, ReadChar}
import backend.IR.InstructionSet._
import backend.IR.Operand._
import frontend.Rules._
import scala.collection.mutable.ListBuffer

object Read {
  /* Obtains the type of an element of a pair. Only works if T is a Pair.
     Returns the type of the first element of pair T if ISFST is true, and the
     second element otherwise. */
  private def getPairElemType(t: Type, isFst: Boolean): Type = t match {
    case Pair(PairElemPair, PairElemPair) => null
    case Pair(PairElemPair, PairElemT(baseType)) =>
      if (isFst) null else baseType
    case Pair(PairElemT(baseType), PairElemPair) =>
      if (isFst) baseType else null
    case Pair(PairElemT(baseTypeFst), PairElemT(baseTypeSnd)) =>
      if (isFst) baseTypeFst else baseTypeSnd
    case _ => ???
  }

  /* Pattern matches on type T and returns the respective BranchLink
     instruction. Adds the respective function to the funcTable. */
  private def readBranch(t: Type): Instruction = t match {
    case CharT =>
      funcTable.addEntry(
        charRead(dataTable.addDataEntry(ReadChar.msgs(0)))
      )
      BranchLink(ReadChar.funcLabel)
    case IntT =>
      funcTable.addEntry(
        intRead(dataTable.addDataEntry(ReadInt.msgs(0)))
      )
      BranchLink(ReadInt.funcLabel)
    // Semantically incorrect
    case _ => ???
  }
  /* Translates read pair elems to the internal representation. */
  def transReadPairElem(
      pe: PairElem,
      isFst: Boolean
  ): ListBuffer[Instruction] = {
    val freeReg = getFreeReg()
    val ident: Ident = pe.e match {
      case id: Ident => id
      case _         => null
    }
    val instructions = transPairElem(ident, isFst, freeReg)
    val (_, pairT) = sTable(ident)
    val t = getPairElemType(pairT, isFst)
    // value must be in R0 for branch
    instructions += Mov(resultReg, freeReg)
    addUnusedReg(freeReg)
    instructions += readBranch(t)
    instructions
  }

  /* Translates read identifiers to the internal representation.
     Only types int and char are semantically valid. */
  private def transReadIdent(ident: Ident): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    val freeReg = getFreeReg()
    val (spIndex, identType) = sTable(ident)
    val spOffset = currentSP - spIndex
    instructions += Add(
      freeReg,
      SP,
      ImmInt(spOffset)
    )
    // variable must be in R0 for the branch
    instructions += Mov(resultReg, freeReg)
    addUnusedReg(freeReg)
    instructions += readBranch(identType)
    instructions
  }

  /* Translates read array-elems to the internal representation. */
  def transReadArrayElem(ae: ArrayElem): ListBuffer[Instruction] = {
    val ArrayElem(ident, exprs, _) = ae
    val instructions = ListBuffer.empty[Instruction]
    val resReg = getFreeReg()
    // Handles nested arrays
    val (_, instrs) = transArrayElem(ident, exprs, resReg)
    instructions ++= instrs
    // value must be in R0 for branch
    instructions += Mov(resultReg, resReg)
    addUnusedReg(resReg)
    // Gets base type of the arrayElem
    val t = getExprType(ae)
    instructions += readBranch(t)
    instructions
  }

  /* Translates read statements to the internal representation. */
  def transRead(lhs: AssignLHS): ListBuffer[Instruction] = {
    lhs match {
      case ident: Ident  => transReadIdent(ident)
      case ae: ArrayElem => transReadArrayElem(ae)
      case fst: Fst      => transReadPairElem(fst, IS_FST_ELEM)
      case snd: Snd      => transReadPairElem(snd, IS_SND_ELEM)
    }
  }

}
