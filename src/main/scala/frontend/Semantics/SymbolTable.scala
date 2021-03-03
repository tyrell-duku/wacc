package frontend.Semantics

import frontend.Rules._

import scala.collection.mutable.HashMap
import scala.collection.mutable

case class Meta(t: Type, pList: Option[List[Type]])

case class SymbolTable(
    parent: SymbolTable,
    funcId: Ident,
    funcMap: HashMap[Ident, Meta]
) {

  val varMap = new HashMap[Ident, (Int, Type)]
  val children = mutable.ListBuffer.empty[SymbolTable]

  def nextScope(nextFunc: Ident): SymbolTable = {
    val next = SymbolTable(this, nextFunc, funcMap)
    children += next
    next
  }

  def nextScope: SymbolTable = {
    val next = SymbolTable(this, funcId, funcMap)
    children += next
    next
  }

  def getNextScope: SymbolTable = {
    val next = children.head
    children -= next
    next
  }

  def getPrevScope: SymbolTable = {
    this.parent
  }

  // Returns Type of variable id & index at which it is stored in SP
  def lookupAllCodeGen(id: Ident): (Int, Type) = {
    var curSymbol = this
    while (curSymbol != null) {
      val dict = curSymbol.varMap
      if (dict.contains(id)) {
        val (index, t) = dict.apply(id)

        // if index is = 0 it has not been declared yet for code gen
        if (index != 0) {
          return (index, t)
        }
      }
      curSymbol = curSymbol.parent
    }
    null
  }

  // Returns Type of id if is a variable else gives the return type of the function id
  def lookupAllType(id: Ident): Type = {
    var curSymbol = this
    while (curSymbol != null) {
      val d = curSymbol.varMap
      if (d.contains(id)) {
        return d.apply(id)
      }
      curSymbol = curSymbol.parent
    }

    funcMap.get(id) match {
      case Some(meta) => meta.t
      case None       => null
    }
  }

  // throw error
  def apply(id: Ident): (Int, Type) = {
    lookupAllCodeGen(id)
  }

  def add(id: Ident, n: Int, t: Type): Unit = {
    varMap.addOne(id, (n, t))
  }

  def add(id: Ident, t: Type): Unit = {
    varMap.addOne(id, (0, t))
  }

  def contains(id: Ident): Boolean = {
    lookupAllType(id) != null
  }

  def containScope(id: Ident): Boolean = {
    varMap.contains(id)
  }

  // Add all variable declarations to dictionary
  def addVars(vars: List[(Ident, Type)]): mutable.ListBuffer[SemanticError] = {
    var semErrors = mutable.ListBuffer.empty[SemanticError]
    for (v <- vars) {
      if (varMap.contains(v._1)) {
        semErrors += VariableDeclared(v._1)
      } else {
        add(v._1, v._2)
      }
    }
    semErrors
  }

  // Add all function declarations to funcMap
  def addFuncs(
      funcs: List[(Ident, Meta)]
  ): mutable.ListBuffer[SemanticError] = {
    var semErrors = mutable.ListBuffer.empty[SemanticError]
    for (f <- funcs) {
      if (funcMap.contains(f._1)) {
        semErrors += FunctionDeclared(f._1)
      } else {
        funcMap.addOne(f)
      }
    }
    semErrors
  }

  def getFuncRetType: Type = {
    if (funcId == null) {
      return null
    }
    val funcRet = funcMap.get(funcId)
    if (funcRet.isEmpty) {
      return null
    }
    funcRet.get.t
  }

  def isFunc(id: Ident): Boolean = {
    val meta = funcMap.get(id)
    meta.isDefined
  }

  //  Ensures params match when calling a function
  def funcParamMatch(
      id: Ident,
      args: Option[ArgList]
  ): mutable.ListBuffer[SemanticError] = {
    val meta = funcMap.get(id)
    if (meta.isEmpty) {
      return mutable.ListBuffer[SemanticError](FunctionNotDeclared(id: Ident))
    }
    val Some(Meta(_, value)) = meta
    if (args.isEmpty) {
      if (value.exists(_.isEmpty)) {
        return mutable.ListBuffer[SemanticError]()
      }
      return mutable.ListBuffer[SemanticError](
        InvalidParams(id, 0, value.get.length)
      )
    }
    val argList = args.get.args
    val pList = value.get
    val paramLen = pList.length
    val argLen = argList.length
    // false if number of arguments > number of parameters
    if (argLen != paramLen) {
      return mutable.ListBuffer[SemanticError](
        InvalidParams(id, argLen, paramLen)
      )
    }
    var result = mutable.ListBuffer.empty[SemanticError]
    for (i <- 0 until argLen) {
      val argType = argList(i).getType(this)
      val paramType = pList(i)
      if (argType != paramType) {
        result += TypeMismatch(argList(i), argType, List(paramType))
      }
    }
    result
  }
}
