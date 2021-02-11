import scala.collection.mutable.HashMap
import Rules._

object SemanticChecker {
  var funcTable = new HashMap[Ident, Type]

  var identTable = new HashMap[Ident, Type]

  def convertToTable(f: Func): (Ident, Meta) = {
    var Func(t, i, ps, _) = f
    (i, Meta(t, ps))
  }

  def progAnalysis(p: Program) = {
    var Program(funcs, s) = p
    var globalTable = SymbolTable(null, new HashMap[Ident, Meta])
    var globalFuncs = funcs.map(convertToTable)
    globalTable.dict.addAll(globalFuncs)

    for (f <- funcs) {
      funcAnalysis(f, SymbolTable(globalTable, new HashMap[Ident, Meta]))
    }

    statAnalysis(s, globalTable)

  }

  def getParam(p: Param): (Ident, Type) = {
    val Param(t, i) = p
    (i, t)
  }

  def funcAnalysis(f: Func, table: SymbolTable) = {
    val Func(t, i, ps, s) = f
    if (!ps.isEmpty) {
      val ParamList(pList) = ps.getOrElse()

      table.addAll(pList.map(p => getParam(p)))
    }
    statAnalysis(s, table)
  }

  def statAnalysis(s: Stat, sTable: SymbolTable): Unit = s match {
    case EqIdent(t, i, r) =>
      if (t == r.getType(sTable) && !identTable.contains(i))
        identTable.addOne(i, t)
      else println("ERROR")
    case EqAssign(l, r) =>
      l match {
        case Ident(v) =>
          if (
            !identTable.contains(Ident(v)) || Ident(v).getType(sTable) != r
              .getType(sTable)
          ) println("ERROR")
        case ArrayElem(Ident(x), y) =>
          if (
            !identTable.contains(Ident(x)) || Ident(x).getType(sTable) != r
              .getType(sTable)
          ) println("ERROR")
        case Fst(Ident(x)) =>
          val rT: Type = r.getType(sTable)
          Ident(x).getType(sTable) match {
            case Pair(PairElemT(t), _) => if (t != rT) println("ERROR")
            case Pair(PairElemPair, _) =>
              rT match {
                case Pair(_, _) =>
                case _          => println("ERROR")
              }
            case _ => println("ERROR")
          }
        case Snd(Ident(x)) =>
          val rT: Type = r.getType(sTable)
          Ident(x).getType(sTable) match {
            case Pair(_, PairElemT(t)) => if (t != rT) println("ERROR")
            case Pair(_, PairElemPair) =>
              rT match {
                case Pair(_, _) =>
                case _          => println("ERROR")
              }
            case _ => println("ERROR")
          }
        case _ => println("ERROR")
      }
    case Read(x) =>
      x match {
        case Ident(v) =>
          Ident(v).getType(sTable) match {
            case Pair(_, _) | ArrayT(_) =>
            case _                      => println("ERROR")
          }
        case _ => println("ERROR")
      }
    case Free(x) =>
      x.getType(sTable) match {
        case Pair(_, _) | ArrayT(_) =>
        case _                      => println("ERROR")
      }
    // case Return(x)      => // TODO: type check x ?
    case Exit(x)    => if (x.getType(sTable) != IntT) { println("ERROR") }
    case Print(x)   => if (x.getType(sTable) == Err) { println("ERROR") }
    case PrintLn(x) => if (x.getType(sTable) == Err) { println("ERROR") }
    case If(x, s1, s2) =>
      if (x.getType(sTable) != BoolT) { println("ERROR") }
    case While(x, s) => if (x.getType(sTable) != BoolT) { println("ERROR") }
    case Seq(x)      => x.map(s => statAnalysis(s, sTable))
    case _           => // ignore Skip
  }

}
