package backend.CodeGeneration

import backend.CodeGeneration.Arrays._
import backend.CodeGeneration.Expressions.transExp
import backend.CodeGeneration.Functions.transCall
import backend.CodeGeneration.MemoryAllocs.transMemoryAlloc
import backend.CodeGeneration.Pairs._
import backend.CodeGenerator._
import backend.IR.InstructionSet._
import backend.IR.Operand._
import frontend.Rules._
import scala.collection.mutable.ListBuffer

object Assignments {

  /* Translates the declaration of a new variable to the internal
     representation. */
  def transEqIdent(
      t: Type,
      id: Ident,
      aRHS: AssignRHS
  ): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    scopeSP += getBaseTypeSize(t)
    sTable.add(id, scopeSP, t)
    val spOffset = currentSP - scopeSP
    val freeReg = getFreeReg()
    val (isByte, instrs) = assignRHS(t, aRHS, freeReg)
    instructions ++= instrs
    instructions += Str(isByte, freeReg, SP, spOffset)
    addUnusedReg(freeReg)
    instructions
  }

  /* Translates the re-assignment of a variable to the internal
     representation. */
  def transEqAssign(
      aLHS: AssignLHS,
      aRHS: AssignRHS
  ): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    val freeReg = getFreeReg()
    aLHS match {
      case Fst(id: Ident, _) =>
        instructions ++= transEqAssignPairElem(aRHS, id, IS_FST_ELEM, freeReg)
      case Snd(id: Ident, _) =>
        instructions ++= transEqAssignPairElem(aRHS, id, IS_SND_ELEM, freeReg)
      case id: Ident =>
        val (index, t) = sTable(id)
        val (isByte, instrs) = assignRHS(t, aRHS, freeReg)
        instructions ++= instrs
        val spOffset = currentSP - index
        instructions += Str(isByte, freeReg, SP, spOffset)
      case ae @ ArrayElem(id, es, _) =>
        val (_, instrs) = assignRHS(getExprType(ae), aRHS, freeReg)
        instructions ++= instrs
        instructions ++= storeArrayElem(id, es, freeReg)
      // Semantically incorrect
      case _ =>
    }
    addUnusedReg(freeReg)
    instructions
  }

  /* Translates the ARHS. Returns if the size of RHS is a byte and the translated
     instructions. */
  def assignRHS(
      t: Type,
      aRHS: AssignRHS,
      freeReg: Reg
  ): (Boolean, ListBuffer[Instruction]) = {
    val instructions = ListBuffer.empty[Instruction]
    aRHS match {
      case ex: Expr => instructions ++= transExp(ex, freeReg)
      // PairElem
      case Fst(id: Ident, _) =>
        instructions ++= transPairElem(id, IS_FST_ELEM, freeReg)
        instructions += loadPairElem(id, freeReg, IS_FST_ELEM)
      case Snd(id: Ident, _) =>
        instructions ++= transPairElem(id, IS_SND_ELEM, freeReg)
        instructions += loadPairElem(id, freeReg, IS_SND_ELEM)
      case Call(id, args, _) =>
        instructions ++= transCall(id, args, freeReg)
      case ArrayLiter(opArr, _) =>
        instructions ++= transArrayLiter(t, opArr, freeReg)
      case Newpair(fst, snd, _) =>
        instructions ++= assignRHSPair(t, fst, snd, freeReg)
      case memAlloc: MemoryAlloc =>
        instructions ++= transMemoryAlloc(t, memAlloc, freeReg)
      // Semantically incorrect
      case _ =>
    }
    (isByte(t), instructions)
  }

}
