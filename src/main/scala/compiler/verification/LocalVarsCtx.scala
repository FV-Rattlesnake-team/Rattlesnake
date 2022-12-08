package compiler.verification

import compiler.verification.LocalVarsCtx.unnamedSymbol
import lang.Types.Type

import scala.collection.mutable

final class LocalVarsCtx private(private val globalVarsCtx: GlobalVarsCtx) {
  private val variables = mutable.Map.empty[String, String]
  
  def currNameFor(rawName: String): String = {
    variables.get(rawName) match
      case Some(name) => name
      case None => throw new NoSuchElementException(s"not found: $rawName")
  }
  
  def addAndRetNameFor(rawName: String, tpe: Type): String = {
    val name = globalVarsCtx.addVarRetName(rawName, tpe)
    variables(rawName) = name
    name
  }

  def currNameForUnnamed: String = currNameFor(unnamedSymbol)

  def addAndRetUnnamedSymbol(tpe: Type): String = addAndRetNameFor(unnamedSymbol, tpe)
  
  def copied: LocalVarsCtx = {
    val newLvc = new LocalVarsCtx(globalVarsCtx)
    newLvc.variables.addAll(variables)
    newLvc
  }

}

object LocalVarsCtx {

  private val unnamedSymbol = "#"

  def newInstance(): LocalVarsCtx = new LocalVarsCtx(new GlobalVarsCtx())
  
}
