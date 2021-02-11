import Rules.Ident
import Rules._

import scala.collection.mutable.HashMap

case class Meta(t: Type, pList: Option[List[Type]])

case class SymbolTable(
    parent: SymbolTable,
    funcId: Ident
) {
  val dict = new HashMap[Ident, Meta]

  def lookupAll(id: Ident): Meta = {
    var curSymbol = this
    while (curSymbol != null) {
      var d = curSymbol.dict
      if (d.contains(id)) {
        return d.apply(id)
      }
      curSymbol = curSymbol.parent
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
    if (funcId == null) {
      return Err
    }
    val funcRet = lookupAll(funcId)
    funcRet.t
  }

  def isFunc(id: Ident): Boolean = {
    val meta = lookupAll(id)
    if (meta == null) {
      return false
    }
    val Meta(_, pList) = meta
    pList != None
  }

  def funcParamMatch(id: Ident, args: Option[ArgList]): Boolean = {
    val meta = lookupAll(id)
    if (meta == null) {
      return false
    }
    val Meta(_, value) = meta
    if (args.isEmpty) {
      return value.exists(_.isEmpty)
    }
    val argList = args.get.args
    val pList = value.get
    val len = argList.length
    // false if number of arguments > number of parameters
    if (len != pList.length) {
      return false
    }

    var valid = true
    for (i <- 0 to len - 1) {
      valid = valid && (argList(i).getType(this) == pList(i))
    }
    valid
  }
}
