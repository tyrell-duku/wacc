package backend.CodeGeneration

import backend.CodeGeneration.Arrays.transArrayElem
import backend.CodeGeneration.Pairs.transPairElem
import backend.CodeGenerator._
import backend.DefinedFuncs.ReadInstructions.{charRead, intRead}
import backend.IR.InstructionSet._
import backend.IR.Operand._
import frontend.Rules._

import scala.collection.mutable.ListBuffer

object Read {

  private def getPairElemType(t: Type, isFst: Boolean): Type = t match {
    case Pair(PairElemPair, PairElemPair) => null
    case Pair(PairElemPair, PairElemT(baseType)) =>
      if (isFst) null else baseType
    case Pair(PairElemT(baseType), PairElemPair) =>
      if (isFst) baseType else null
    case Pair(PairElemT(baseTypeFst), PairElemT(baseTypeSnd)) =>
      if (isFst) baseTypeFst else baseTypeSnd
    case _ => null
  }

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
    instructions += Mov(R0, freeReg)
    addUnusedReg(freeReg)
    t match {
      case CharT =>
        instructions += BranchLink(Label("p_read_char"))
        funcTable.addEntry(charRead(dataTable.addDataEntry(" %c\\0")))
      case IntT =>
        instructions += BranchLink(Label("p_read_int"))
        funcTable.addEntry(intRead(dataTable.addDataEntry("%d\\0")))
      // Semantically incorrect
      case _ =>
    }
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
    instructions += Mov(R0, freeReg)
    addUnusedReg(freeReg)
    // pattern matching for which read label to use
    identType match {
      case CharT =>
        instructions += BranchLink(Label("p_read_char"))
        funcTable.addEntry(charRead(dataTable.addDataEntry(" %c\\0")))
      case IntT =>
        instructions += BranchLink(Label("p_read_int"))
        funcTable.addEntry(intRead(dataTable.addDataEntry("%d\\0")))
      // Semantically incorrect
      case _ => ListBuffer.empty[Instruction]
    }
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
    instructions += Mov(R0, resReg)
    addUnusedReg(resReg)
    // Gets base type of the arrayElem
    val t = getExprType(ae)
    t match {
      case CharT =>
        instructions += BranchLink(Label("p_read_char"))
        funcTable.addEntry(charRead(dataTable.addDataEntry(" %c\\0")))
      case IntT =>
        instructions += BranchLink(Label("p_read_int"))
        funcTable.addEntry(intRead(dataTable.addDataEntry("%d\\0")))
      // Semantically incorrect
      case _ =>
    }
    instructions
  }

  /* Translates read statements to the internal representation. */
  def transRead(lhs: AssignLHS): ListBuffer[Instruction] = {
    lhs match {
      case ident: Ident  => transReadIdent(ident)
      case ae: ArrayElem => transReadArrayElem(ae)
      case fst: Fst      => transReadPairElem(fst, true)
      case snd: Snd      => transReadPairElem(snd, false)
    }
  }

}
