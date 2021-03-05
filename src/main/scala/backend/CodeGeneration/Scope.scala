package backend.CodeGeneration

import backend.CodeGeneration.Expressions.transExp
import backend.CodeGenerator._
import backend.IR.InstructionSet._
import backend.IR.Operand.ImmInt
import frontend.Rules.{Expr, Stat}

import scala.collection.mutable.ListBuffer

object Scope {

  def transBegin(
      s: Stat,
      curInstrs: ListBuffer[Instruction]
  ): ListBuffer[Instruction] = {
    transNextScope(s, curInstrs)
  }

  private def transNextScope(
      s: Stat,
      curInstrs: ListBuffer[Instruction]
  ): ListBuffer[Instruction] = {
    val oldScopeSP = scopeSP
    sTable = sTable.getNextScope
    val curScopeMaxSPDepth = sTable.spMaxDepth
    curInstrs ++= subSP(curScopeMaxSPDepth)
    scopeSP = currentSP
    currentSP += curScopeMaxSPDepth
    val instructions = transStat(s, curInstrs)
    if (curScopeMaxSPDepth > 0) {
      instructions ++= addSP(curScopeMaxSPDepth)
      currentSP -= curScopeMaxSPDepth
    }
    scopeSP = oldScopeSP
    sTable = sTable.getPrevScope
    instructions
  }

  def transIf(
      cond: Expr,
      s1: Stat,
      s2: Stat,
      curInstrs: ListBuffer[Instruction]
  ): ListBuffer[Instruction] = {
    val reg = getFreeReg()
    curInstrs ++= transExp(cond, reg)
    // check if condition is false
    curInstrs += Cmp(reg, ImmInt(0))
    addUnusedReg(reg)
    val elseBranch = assignLabel()
    curInstrs += BranchEq(elseBranch)
    // statement if the condition was true
    val instructions = transNextScope(s1, curInstrs)
    val afterLabel = assignLabel()
    instructions += Branch(afterLabel)
    userFuncTable.addEntry(currentLabel, instructions.toList)
    currentLabel = elseBranch
    val elseInstrs = transNextScope(s2, ListBuffer.empty[Instruction])
    userFuncTable.addEntry(
      currentLabel,
      elseInstrs.toList
    )
    currentLabel = afterLabel
    ListBuffer.empty[Instruction]
  }

  def transWhile(
      cond: Expr,
      s: Stat,
      curInstrs: ListBuffer[Instruction]
  ): ListBuffer[Instruction] = {
    val instructions = ListBuffer.empty[Instruction]
    val afterLabel = assignLabel()
    curInstrs += Branch(afterLabel)
    userFuncTable.addEntry(currentLabel, curInstrs.toList)
    val insideWhile = assignLabel()
    currentLabel = insideWhile
    val transInnerWhile = transNextScope(s, ListBuffer.empty[Instruction])
    userFuncTable.addEntry(currentLabel, transInnerWhile.toList)
    currentLabel = afterLabel
    val reg = getFreeReg()
    instructions ++= transExp(cond, reg)
    // check if condition is true
    instructions += Cmp(reg, ImmInt(1))
    addUnusedReg(reg)
    instructions += BranchEq(insideWhile)
  }
}
