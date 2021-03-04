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
  final val allRegs: ListBuffer[Reg] =
    ListBuffer(R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12)

  var freeRegs = allRegs
  final val resultReg: Reg = R0
  freeRegs -= resultReg
  freeRegs -= R1
  freeRegs -= R2
  freeRegs -= R3

  var currentSP = 0
  var sTable: SymbolTable = null
  var scopeSP = 0
  var currentLabel = Label("main")

  private var labelCounter = 0

  val dataTable = new DataTable
  val userFuncTable = new FuncTable
  val funcTable = new FuncTable

  val INT_SIZE = 4
  val CHAR_SIZE = 1
  val BOOL_SIZE = 1
  val STR_SIZE = 4
  val ARRAY_SIZE = 4
  val PAIR_SIZE = 4
  val MAX_INT_IMM = 1024

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
    val Program(funcs, stat) = prog
    for (f <- funcs) {
      transFunc(f)
    }
    this.sTable = sTable
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
    userFuncTable.addEntry(currentLabel, instructions.toList)
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
      case Print(e)         => instructions ++= transPrint(e, false)
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
      case IntLiter(_, _)  => IntT
      case BoolLiter(_, _) => BoolT
      case CharLiter(_, _) => CharT
      case StrLiter(_, _)  => StringT
      case PairLiter(_)    => Pair(null, null)
      case id: Ident =>
        val (_, t) = sTable(id)
        t
      case ArrayElem(id, es, _) =>
        var (_, t) = sTable(id)
        for (_ <- es) {
          t = getInnerType(t)
        }
        t
      case Not(_, _)      => BoolT
      case Negation(_, _) => IntT
      case Len(_, _)      => IntT
      case Ord(_, _)      => IntT
      case Chr(_, _)      => CharT
      case _: ArithOps    => IntT
      case _: ComparOps   => BoolT
      case _: EqOps       => BoolT
      case _: LogicalOps  => BoolT
    }
  }

  def assignLabel(): Label = {
    val nextLabel = Label("L" + labelCounter)
    labelCounter += 1
    nextLabel
  }

  private def transExit(
      e: Expr
  ): ListBuffer[Instruction] = {
    val freeReg = getFreeReg()
    val instructions = transExp(e, freeReg)
    instructions ++= ListBuffer[Instruction](
      Mov(R0, freeReg),
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
      instrs += backend.IR.InstructionSet.Sub(SP, SP, ImmInt(MAX_INT_IMM))
    }
    instrs += backend.IR.InstructionSet.Sub(SP, SP, ImmInt(curSp))
    instrs
  }

  def isByte(t: Type): Boolean = {
    t == BoolT || t == CharT
  }

  def getFreeReg(): Reg = {
    if (freeRegs.isEmpty) {
      // TODO: free up regs?
      return R0
    }
    val reg = freeRegs(0)
    freeRegs.remove(0)
    reg
  }

  def addUnusedReg(r: Reg): Unit = {
    r +=: freeRegs
  }

  def boolToInt(b: Boolean): Int = {
    if (b) { 1 }
    else { 0 }
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
