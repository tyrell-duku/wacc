package backend.CodeGeneration

import backend.CodeGeneration.Expressions.transExp
import backend.CodeGenerator._
import backend.IR.InstructionSet._
import backend.IR.Operand.ImmInt
import backend.IR.Condition.EQ
import frontend.Rules.{Expr, Stat}

import scala.collection.mutable.ListBuffer

object Scope {

  private final val TRUE_CMP_INT = ImmInt(1)

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
    val elseBranch = funcTable.assignLabel()
    curInstrs += BranchCond(EQ, elseBranch)
    // statement if the condition was true
    val instructions = transNextScope(s1, curInstrs)
    val afterLabel = funcTable.assignLabel()
    instructions += Branch(afterLabel)
    userFuncTable.addEntry(currentLabel, instructions)
    currentLabel = elseBranch
    val elseInstrs = transNextScope(s2, ListBuffer.empty[Instruction])
    userFuncTable.addEntry(
      currentLabel,
      elseInstrs
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
    val afterLabel = funcTable.assignLabel()
    curInstrs += Branch(afterLabel)
    userFuncTable.addEntry(currentLabel, curInstrs)
    val insideWhile = funcTable.assignLabel()
    currentLabel = insideWhile
    val transInnerWhile = transNextScope(s, ListBuffer.empty[Instruction])
    userFuncTable.addEntry(currentLabel, transInnerWhile)
    currentLabel = afterLabel
    val reg = getFreeReg()
    instructions ++= transExp(cond, reg)
    // check if condition is true
    instructions += Cmp(reg, TRUE_CMP_INT)
    addUnusedReg(reg)
    instructions += BranchCond(EQ, insideWhile)
  }
}
