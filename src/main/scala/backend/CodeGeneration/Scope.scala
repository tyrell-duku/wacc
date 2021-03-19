package backend.CodeGeneration

import backend.CodeGeneration.Expressions.transExp
import backend.CodeGenerator._
import backend.IR.InstructionSet._
import backend.IR.Operand.ImmInt
import backend.IR.Condition.EQ
import frontend.Rules.{Expr, Stat}

import scala.collection.mutable.ListBuffer

object Scope {
  /* Translates a begin statement into the IR */
  def transBegin(
      s: Stat,
      curInstrs: ListBuffer[Instruction]
  ): ListBuffer[Instruction] = {
    transNextScope(s, curInstrs)
  }

  /* Translates the code within the next scope. CURINSTRS contains the
     instructions that have been translated until the point where transNextScope
     has been called. */
  private def transNextScope(
      s: Stat,
      curInstrs: ListBuffer[Instruction]
  ): ListBuffer[Instruction] = {
    sTable = sTable.getNextScope
    val instructions = transStat(s, curInstrs)
    sTable = sTable.getPrevScope
    instructions
  }

  /* Translates the if statement to the internal representation.
     CURINSTRS are the instructions executed before this function,
     used for the if branching. */
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
  /* Translates a while-loop with condition COND and body S into the
     internal representation given the instructions that had been translated
     beforehand CURINSTRS. */
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
    instructions += Cmp(reg, ImmInt(TRUE_INT))
    addUnusedReg(reg)
    instructions += BranchCond(EQ, insideWhile)
  }
}
