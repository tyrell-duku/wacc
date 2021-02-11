import Rules.Ident
import Rules._

import scala.collection.mutable.HashMap

case class Meta(t: Type, value: Any)

case class SymbolTable(
    parent: SymbolTable,
    funcId: Ident
) {
  val dict = new HashMap[Ident, Meta]
  def lookupAll(id: Ident): Meta = {
    var curSymbol = this
    var d = curSymbol.dict
    while (d != null) {
      if (d.contains(id)) {
        return d.apply(id)
      }
      curSymbol = this.parent
      d = curSymbol.dict
    }
    null
  }

  def add(id: Ident, t: Type): Unit = {
    dict.addOne(id, Meta(t, None))
  }

  def contains(id: Ident): Boolean = {
    lookupAll(id) != null
  }

  def containScope(id: Ident): Boolean = {
    dict.contains(id)
  }

  def addAll(entries: List[(Ident, Type)]) {
    val toMeta = entries.map((x: (Ident, Type)) => (x._1, Meta(x._2, None)))
    dict.addAll(toMeta)
  }

  def getFuncRetType: Type = {
    lookupAll(funcId).t
  }

}
