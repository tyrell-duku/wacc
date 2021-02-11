import scala.collection.mutable.HashMap
import Rules._
import Parser._

object SemanticChecker {
  // var symbolTable = new HashMap[Ident, Meta]
  // sealed class Meta(t: Type, value: Any)

  var funcTable = new HashMap[Ident, Type]

  var identTable = new HashMap[Ident, Type]

  def analysis(p: Program): Unit = {}

  def funcAnalysis(f: Func) = {
    // analyse each statement in the function up to return/exit
  }

  def statAnalysis(s: Stat) = s match {
    case EqIdent(t, i, r) =>
      if (t == r.getType(identTable) && !identTable.contains(i))
        identTable.addOne(i, t)
      else println("ERROR")
    case EqAssign(l, r) =>
      l match {
        case Ident(v) =>
          if (
            !identTable.contains(Ident(v)) || Ident(v).getType(identTable) != r
              .getType(identTable)
          ) println("ERROR")
        case ArrayElem(Ident(x), y) =>
          if (
            !identTable.contains(Ident(x)) || Ident(x).getType(identTable) != r
              .getType(identTable)
          ) println("ERROR")
        case Fst(Ident(x)) =>
          val rT: Type = r.getType(identTable)
          Ident(x).getType(identTable) match {
            case Pair(PairElemT(t), _) => if (t != rT) println("ERROR")
            case Pair(PairElemPair, _) =>
              rT match {
                case Pair(_, _) =>
                case _          => println("ERROR")
              }
            case _ => println("ERROR")
          }
        case Snd(Ident(x)) =>
          val rT: Type = r.getType(identTable)
          Ident(x).getType(identTable) match {
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
          Ident(v).getType(identTable) match {
            case Pair(_, _) | ArrayT(_) =>
            case _                      => println("ERROR")
          }
        case _ => println("ERROR")
      }
    case Free(x) =>
      x.getType(identTable) match {
        case Pair(_, _) | ArrayT(_) =>
        case _                      => println("ERROR")
      }
    // case Return(x)      => // TODO: type check x ?
    case Exit(x)    => if (x.getType(identTable) != IntT) { println("ERROR") }
    case Print(x)   => if (x.getType(identTable) == NA) { println("ERROR") }
    case PrintLn(x) => if (x.getType(identTable) == NA) { println("ERROR") }
    case If(x, s1, s2) =>
      if (x.getType(identTable) != BoolT) { println("ERROR") }
    case While(x, s) => if (x.getType(identTable) != BoolT) { println("ERROR") }
    case _           => // ignore Skip
  }

}
