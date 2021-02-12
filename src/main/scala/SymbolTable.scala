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

  // Add all function declarations to dictionary
  def addAll(entries: List[(Ident, Meta)]) {
    for (m <- entries) {
      if (dict.contains(m._1)) {
        println("Conflicting function declarations at " + m._1.s)
      } else {
        dict.addOne(m)
      }
    }
  }

  def getFuncRetType: Type = {
    if (funcId == null) {
      return null
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
    pList.isDefined
  }

  def funcParamMatch(id: Ident, args: Option[ArgList]): List[SemanticError] = {
    val meta = lookupAll(id)
    if (meta == null) {
      return List[SemanticError](functionNotDeclared(id: Ident))
    }
    val Meta(_, value) = meta
    if (args.isEmpty) {
      if (value.exists(_.isEmpty)) {
        return List[SemanticError]()
      }
      return List(invalidParams(id, 0, value.get.length))
    }
    val argList = args.get.args
    val pList = value.get
    val paramLen = pList.length
    val argLen = argList.length
    // false if number of arguments > number of parameters
    if (argLen != paramLen) {
      return List(invalidParams(id, argLen, paramLen))
    }
    var result = List[SemanticError]()
    for (i <- 0 until argLen) {
      val argType = argList(i).getType(this)
      val paramType = pList(i)
      if (argType != paramType) {
        result ::= typeMismatch(argList(i), argType, List(paramType))
      }
    }
    result
  }
}
