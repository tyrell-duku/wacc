package backend.CodeGeneration

import backend.CodeGeneration.Arrays.storeArrayElem
import backend.CodeGeneration.Expressions.transExp
import backend.CodeGeneration.Functions.transCall
import backend.CodeGeneration.Pairs._
import backend.CodeGenerator._
import backend.IR.InstructionSet._
import backend.IR.Operand._
import frontend.Rules._

import scala.collection.mutable.ListBuffer

object Assignments {

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
    if (isByte) {
      instructions += StrB(freeReg, RegisterOffset(SP, spOffset))
    } else {
      instructions += Str(freeReg, RegisterOffset(SP, spOffset))
    }

    addUnusedReg(freeReg)
    instructions
  }

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
        instructions ++= transPairElem(id, true, freeReg)
        instructions += loadPairElem(id, freeReg, true)
      case Snd(id: Ident, _) =>
        instructions ++= transPairElem(id, false, freeReg)
        instructions += loadPairElem(id, freeReg, false)
      case Call(id, args, _) =>
        instructions ++= transCall(id, args, freeReg)
      case ArrayLiter(opArr, _) =>
        val ArrayT(innerType) = t
        val arr = opArr match {
          case Some(arr) => arr
          case None      => List.empty[Expr]
        }

        val listSize = arr.size
        val baseTypeSize = getBaseTypeSize(innerType)
        val sizeToMalloc = 4 + (listSize * baseTypeSize)
        instructions += Ldr(R0, ImmMem(sizeToMalloc))
        instructions += BranchLink(Label("malloc"))
        instructions += Mov(freeReg, R0)
        val nextFreeReg = getFreeReg()

        if (innerType == CharT || innerType == BoolT) {
          for (i <- 0 until listSize) {
            instructions ++= transExp(arr(i), nextFreeReg)
            instructions += StrB(
              nextFreeReg,
              RegisterOffset(freeReg, i + 4)
            )
          }
        } else {
          for (i <- 0 until listSize) {
            instructions ++= transExp(arr(i), nextFreeReg)
            instructions += Str(
              nextFreeReg,
              RegisterOffset(freeReg, (i + 1) * 4)
            )
          }
        }
        instructions += Ldr(nextFreeReg, ImmMem(listSize))
        instructions += Str(nextFreeReg, RegAdd(freeReg))
        addUnusedReg(nextFreeReg)
      case Newpair(fst, snd, _) =>
        instructions ++= assignRHSPair(t, fst, snd, freeReg)
      case _ =>
    }
    (isByte(t), instructions)
  }

  def transEqAssign(
      aLHS: AssignLHS,
      aRHS: AssignRHS
  ): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    val freeReg = getFreeReg()
    aLHS match {
      case Fst(id: Ident, _) =>
        instructions ++= transEqAssignPairElem(aRHS, id, true, freeReg)
      case Snd(id: Ident, _) =>
        instructions ++= transEqAssignPairElem(aRHS, id, false, freeReg)
      case id: Ident =>
        val (index, t) = sTable(id)
        val (isByte, instrs) = assignRHS(t, aRHS, freeReg)
        instructions ++= instrs
        val spOffset = currentSP - index

        if (isByte) {
          instructions += StrB(freeReg, RegisterOffset(SP, spOffset))
        } else {
          instructions += Str(freeReg, RegisterOffset(SP, spOffset))
        }

      case ae @ ArrayElem(id, es, _) =>
        val (_, instrs) = assignRHS(getExprType(ae), aRHS, freeReg)
        instructions ++= instrs
        instructions ++= storeArrayElem(id, es, freeReg)
      case _ =>
    }
    addUnusedReg(freeReg)
    instructions
  }

}
