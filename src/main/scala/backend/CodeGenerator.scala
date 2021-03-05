package backend

import backend.CodeGeneration.Assignments._
import backend.CodeGeneration.Expressions._
import backend.CodeGeneration.Free.transFree
import backend.CodeGeneration.Functions._
import backend.CodeGeneration.Print.transPrint
import backend.CodeGeneration.Read.transRead
import backend.CodeGeneration.Scope._
import frontend.Rules._
import frontend.Semantics.SymbolTable
import scala.collection.mutable.ListBuffer
import backend.DataTypes.{DataTable, FuncTable}
import backend.IR.InstructionSet._
import backend.IR.Operand._

object CodeGenerator {
  /* Registers */
  final val allRegs: ListBuffer[Reg] =
    ListBuffer(R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10)

  var freeRegs = allRegs
  final val resultReg: Reg = R0
  final val popReg = R11
  freeRegs -= resultReg
  freeRegs -= R1
  freeRegs -= R2
  freeRegs -= R3

  var currentSP = 0
  var sTable: SymbolTable = null
  var scopeSP = 0
  var currentLabel = Label("main")
  val dataTable = new DataTable
  val userFuncTable = new FuncTable
  val funcTable = new FuncTable

  /* Type sizes */
  val INT_SIZE = 4
  val CHAR_SIZE = 1
  val BOOL_SIZE = 1
  val STR_SIZE = 4
  val ADDRESS_SIZE = 4
  val ARRAY_SIZE = ADDRESS_SIZE
  val PAIR_SIZE = ADDRESS_SIZE
  val MAX_INT_IMM = 1024

  val NO_OFFSET = 0
  val IS_FST_ELEM = true
  val IS_SND_ELEM = false

  val TRUE_CMP_INT = ImmInt(1)
  val FALSE_CMP_INT = ImmInt(0)

  private def saveRegs(
      regsNotInUse: ListBuffer[Reg]
  ): Instruction = {
    val regsToPush = allRegs.filter(r => !regsNotInUse.contains(r))
    Push(regsToPush)
  }

  private def restoreRegs(
      regsNotInUse: ListBuffer[Reg]
  ): Instruction = {
    val regsToPush = allRegs.filter(r => !regsNotInUse.contains(r)).reverse
    new Pop(regsToPush)
  }

  def transProg(
      prog: Program,
      sTable: SymbolTable
  ): (List[Data], List[(Label, List[Instruction])]) = {
    this.sTable = sTable
    val Program(funcs, stat) = prog
    for (f <- funcs) {
      transFunc(f)
    }
    currentLabel = Label("main")
    scopeSP = currentSP
    val curScopeMaxSPDepth = sTable.spMaxDepth
    currentSP += curScopeMaxSPDepth
    val instructions = transStat(
      stat,
      Push(ListBuffer(LR)) +=: subSP(curScopeMaxSPDepth)
    )
    var toAdd = addSP(currentSP) ++ ListBuffer(
      Ldr(resultReg, ImmMem(0)),
      Pop(ListBuffer(PC)),
      Ltorg
    )
    instructions ++= toAdd
    userFuncTable.addEntry(currentLabel, instructions)
    val funcList = userFuncTable.table ++ funcTable.table
    (dataTable.table.toList, funcList.toList)
  }

  def transStat(
      stat: Stat,
      instructions: ListBuffer[Instruction]
  ): ListBuffer[Instruction] = {
    stat match {
      case EqIdent(t, i, r) => instructions ++= transEqIdent(t, i, r)
      case EqAssign(l, r)   => instructions ++= transEqAssign(l, r)
      case Read(lhs)        => instructions ++= transRead(lhs)
      case Free(id: Ident)  => instructions ++= transFree(id)
      case Return(e)        => instructions ++= transReturn(e)
      case Exit(e)          => instructions ++= transExit(e)
      // isNewLine = false
      case Print(e) => instructions ++= transPrint(e, false)
      // isNewLine = true
      case PrintLn(e)       => instructions ++= transPrint(e, true)
      case If(cond, s1, s2) => transIf(cond, s1, s2, instructions)
      case While(cond, s)   => transWhile(cond, s, instructions)
      case Seq(statList) =>
        var nextInstructions = instructions
        for (s <- statList) {
          nextInstructions = transStat(s, nextInstructions)
        }
        nextInstructions
      case Begin(s) => transBegin(s, instructions)
      case _        => instructions
    }
  }

  def getInnerType(t: Type): Type = t match {
    case ArrayT(inner) => inner
    // invalid case, will never enter
    case _ => null
  }

  def getExprType(e: Expr): Type = {
    e match {
      case _: IntLiter  => IntT
      case _: BoolLiter => BoolT
      case _: CharLiter => CharT
      case _: StrLiter  => StringT
      case _: PairLiter => Pair(null, null)
      case id: Ident =>
        val (_, t) = sTable(id)
        t
      case ArrayElem(id, es, _) =>
        var (_, t) = sTable(id)
        for (_ <- es) {
          t = getInnerType(t)
        }
        t
      case _: Not        => BoolT
      case _: Negation   => IntT
      case _: Len        => IntT
      case _: Ord        => IntT
      case _: Chr        => CharT
      case _: ArithOps   => IntT
      case _: ComparOps  => BoolT
      case _: EqOps      => BoolT
      case _: LogicalOps => BoolT
    }
  }

  private def transExit(
      e: Expr
  ): ListBuffer[Instruction] = {
    val freeReg = getFreeReg()
    val instructions = transExp(e, freeReg)
    instructions ++= ListBuffer[Instruction](
      Mov(resultReg, freeReg),
      BranchLink(Label("exit"))
    )
    addUnusedReg(freeReg)
    instructions
  }

  def addSP(spToAdd: Int): ListBuffer[Instruction] = {
    val instrs = ListBuffer.empty[Instruction]
    if (spToAdd == 0) {
      return instrs
    }
    var curSp = spToAdd
    while (curSp > MAX_INT_IMM) {
      curSp -= MAX_INT_IMM
      instrs += Add(SP, SP, ImmInt(MAX_INT_IMM))
    }
    instrs += Add(SP, SP, ImmInt(curSp))
    instrs
  }

  def subSP(spToSub: Int): ListBuffer[Instruction] = {
    val instrs = ListBuffer.empty[Instruction]
    if (spToSub == 0) {
      return instrs
    }
    var curSp = spToSub
    while (curSp > MAX_INT_IMM) {
      curSp -= MAX_INT_IMM
      instrs += IR.InstructionSet.Sub(SP, SP, ImmInt(MAX_INT_IMM))
    }
    instrs += IR.InstructionSet.Sub(SP, SP, ImmInt(curSp))
    instrs
  }

  def isByte(t: Type): Boolean = {
    t == BoolT || t == CharT
  }

  def getFreeReg(): Reg = {
    if (freeRegs.isEmpty) {
      return popReg
    }
    val reg = freeRegs(0)
    freeRegs.remove(0)
    reg
  }

  def addUnusedReg(r: Reg): Unit = {
    r +=: freeRegs
  }

  def boolToInt(b: Boolean): Int = {
    if (b) 1 else 0
  }

  def getBaseTypeSize(t: Type): Int = {
    t match {
      case IntT           => INT_SIZE
      case BoolT          => BOOL_SIZE
      case CharT          => CHAR_SIZE
      case StringT        => STR_SIZE
      case ArrayT(innerT) => ARRAY_SIZE
      case Pair(_, _)     => PAIR_SIZE
      case _              => -1
    }
  }

}
