package backend

import InstructionSet._
import frontend.Rules._
import scala.collection.mutable.ListBuffer

object CodeGenerator {
  private var instructions: ListBuffer[Instruction] =
    ListBuffer.empty[Instruction]

  final val allRegs: ListBuffer[Reg] =
    ListBuffer(R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12)

  final val resultReg: Reg = R0

  var varTable = Map.empty[Ident, Reg]

  private val dataTable = new DataTable

  private val INT_SIZE = 4
  private val CHAR_SIZE = 1
  private val BOOL_SIZE = 1
  private val STR_SIZE = 4
  private val ARRAY_SIZE = 4

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
      prog: Program
  ): (List[Data], List[(Label, List[Instruction])]) = {
    val Program(funcs, stat) = prog
    // val funcCode = funcs.map(transFunc)
    // instructions += funcCode
    instructions = ListBuffer(Push(ListBuffer(LR)))
    var toAdd =
      transStat(stat, allRegs) ++ ListBuffer(
        Ldr(resultReg, ImmMem(0)),
        Pop(ListBuffer(PC))
      )
    val data = List.empty[Data]
    instructions ++= toAdd
    (data, List((Label("main"), instructions.toList)))
  }

  private def transStat(
      stat: Stat,
      regs: ListBuffer[Reg]
  ): ListBuffer[Instruction] = {
    stat match {
      case EqIdent(t, i, r) => transEqIdent(t, i, r, regs)
      case EqAssign(l, r)   => ListBuffer.empty[Instruction]
      case Read(lhs)        => ListBuffer.empty[Instruction]
      case Free(e)          => ListBuffer.empty[Instruction]
      case Return(e)        => ListBuffer.empty[Instruction]
      case Exit(e)          => transExit(e, regs)
      case Print(e)         => ListBuffer.empty[Instruction]
      case PrintLn(e)       => ListBuffer.empty[Instruction]
      case If(cond, s1, s2) => ListBuffer.empty[Instruction]
      case While(cond, s)   => ListBuffer.empty[Instruction]
      case Begin(s)         => ListBuffer.empty[Instruction]
      case Seq(statList)    => ListBuffer.empty[Instruction]
      case _                => ListBuffer.empty[Instruction]
    }
  }

  private def transExit(
      e: Expr,
      regs: ListBuffer[Reg]
  ): ListBuffer[Instruction] = {
    e match {
      case IntLiter(n, _) =>
        val freeReg = getFreeReg(regs, ListBuffer.empty[Reg])
        ListBuffer[Instruction](
          Ldr(freeReg, ImmMem(n)),
          Mov(R0, freeReg),
          BranchLink(Label("exit"))
        )
      case _ => transExp(e)
    }
  }

  private def transEqIdent(
      t: Type,
      id: Ident,
      aRHS: AssignRHS,
      regs: ListBuffer[Reg]
  ): ListBuffer[Instruction] = {
    ListBuffer.empty[Instruction]
  }

  private def transStrLiter(str: List[Character]): ListBuffer[Instruction] = {
    dataTable.addDataEntry(str)
    ListBuffer(Ldr(R1, DataLabel(Label(dataTable.getCurrLabel()))))
  }

  private def transArraryElem(es: List[Expr]): ListBuffer[Instruction] = {
    // TODO: symbol table from semanticChecker required
    val st = null
    val length = es.length
    val typeSize = getBaseTypeSize(es(0).getType(st))
    val lb = ListBuffer.empty[Instruction]

    lb += Ldr(R4, ImmMem(4 + typeSize * length))
    lb += BranchLink(Label("malloc"))
    lb += Mov(R4, R0)

    for (i <- 1 to length) {
      lb += Ldr(R5, ImmMem(0))
      lb += StrOffset(R5, R4, typeSize * i)
    }

    lb += Str(R5, RegAdd(R4))
    lb
  }

  private def transBinOp(op: BinOp): ListBuffer[Instruction] = {
    op match {
      case frontend.Rules.Mul(lExpr, rExpr, _) => ListBuffer.empty
      case Div(lExpr, rExpr, _)                => ListBuffer.empty
      case Mod(lExpr, rExpr, _)                => ListBuffer.empty
      case Plus(lExpr, rExpr, _)               => ListBuffer.empty
      case frontend.Rules.Sub(lExpr, rExpr, _) => ListBuffer.empty
      case GT(lExpr, rExpr, _)                 => ListBuffer.empty
      case GTE(lExpr, rExpr, _)                => ListBuffer.empty
      case LT(lExpr, rExpr, _)                 => ListBuffer.empty
      case LTE(lExpr, rExpr, _)                => ListBuffer.empty
      case Equal(lExpr, rExpr, _)              => ListBuffer.empty
      case NotEqual(lExpr, rExpr, _)           => ListBuffer.empty
      case frontend.Rules.And(lExpr, rExpr, _) => ListBuffer.empty
      case frontend.Rules.Or(lExpr, rExpr, _)  => ListBuffer.empty
    }
  }

  // TODO: Determine free registers
  private def transExp(
      e: Expr
  ): ListBuffer[Instruction] = {
    e match {
      case IntLiter(n, _) => ListBuffer(Mov(R1, ImmInt(n)))
      case BoolLiter(b, _) =>
        return ListBuffer
          .empty[Instruction] // return ListBuffer(Mov(R1, ImmInt(n)))
      // TODO: escaped character
      case CharLiter(c, _)  => ListBuffer(Mov(R1, ImmChar(c)))
      case StrLiter(str, _) => transStrLiter(str)
      case PairLiter(_)     => ListBuffer(Ldr(R1, ImmMem(0)))
      // TODO: track variable location
      case Ident(s, _)          => ListBuffer.empty[Instruction]
      case ArrayElem(id, es, _) => transArraryElem(es)
      case e: UnOp              => ListBuffer.empty[Instruction]
      case e: BinOp             => ListBuffer.empty[Instruction]
    }
  }

  private def getFreeReg(
      regs: ListBuffer[Reg],
      regsInUse: ListBuffer[Reg]
  ): Reg = {
    regs.filter(r => !regsInUse.contains(r))(0)
  }

  private def getBaseTypeSize(t: Type): Int = {
    t match {
      case IntT           => INT_SIZE
      case BoolT          => BOOL_SIZE
      case CharT          => CHAR_SIZE
      case StringT        => STR_SIZE
      case ArrayT(innerT) => ARRAY_SIZE
      case _: PairType    => -1
      case _              => -1
    }
  }

}
