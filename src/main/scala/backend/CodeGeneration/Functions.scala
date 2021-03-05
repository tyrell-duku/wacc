package backend.CodeGeneration

import backend.CodeGeneration.Expressions.transExp
import backend.CodeGenerator._
import backend.IR.InstructionSet._
import backend.IR.Operand._
import frontend.Rules._

import scala.collection.mutable.ListBuffer

object Functions {

  def transCall(
      id: Ident,
      args: Option[ArgList],
      freeReg: Reg
  ): ListBuffer[Instruction] = {
    val (argInstrs, toAdd) = loadArgs(args, freeReg)
    argInstrs += BranchLink(Label("f_" + id))
    argInstrs ++= addSP(toAdd)
    argInstrs += Mov(freeReg, resultReg)
    argInstrs
  }

  private def loadArgs(
      args: Option[ArgList] = None,
      reg: Reg
  ): (ListBuffer[Instruction], Int) = {
    var instrs = ListBuffer.empty[Instruction]
    var totalOff = 0
    args match {
      case None =>
      case Some(ArgList(aList)) =>
        for (e <- aList.reverse) {
          val t = getExprType(e)
          instrs ++= transExp(e, reg)
          instrs += StrOffsetIndex(isByte(t), reg, SP, -getBaseTypeSize(t))
          currentSP += getBaseTypeSize(t)
          totalOff += getBaseTypeSize(t)
        }
    }
    currentSP -= totalOff
    (instrs, totalOff)
  }

  private def transFuncParams(ps: Option[ParamList]): Unit = ps match {
    case None =>
    case Some(ParamList(plist)) =>
      var currSp = 4
      var prevSize = 0
      for (param <- plist) {
        val Param(t, id) = param
        currSp += prevSize
        prevSize = getBaseTypeSize(t)
        sTable.add(id, -currSp, t)
      }
  }

  def transFunc(func: Func): Unit = {
    val Func(t, id, ps, s) = func
    currentLabel = Label("f_" + id)
    val oldScopeSP = scopeSP
    sTable = sTable.getNextScope
    val curScopeMaxSPDepth = sTable.spMaxDepth(id)
    transFuncParams(ps)
    scopeSP = currentSP
    currentSP += curScopeMaxSPDepth
    val instructions = transStat(
      s,
      Push(ListBuffer(LR)) +=: subSP(curScopeMaxSPDepth)
    )
    if (curScopeMaxSPDepth > 0) {
      currentSP -= curScopeMaxSPDepth
    }
    scopeSP = oldScopeSP
    sTable = sTable.getPrevScope
    userFuncTable.addEntry(currentLabel, instructions.toList)
  }

  def transReturn(e: Expr): ListBuffer[Instruction] = {
    val reg = getFreeReg()
    val instructions = transExp(e, reg)
    instructions += Mov(resultReg, reg)
    if (scopeSP > 0) {
      instructions ++= addSP(scopeSP)
    }
    instructions ++= ListBuffer(
      Pop(ListBuffer(PC)),
      Pop(ListBuffer(PC)),
      Ltorg
    )
    addUnusedReg(reg)
    instructions
  }

}
