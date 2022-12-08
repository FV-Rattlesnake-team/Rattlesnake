package compiler.verification

import lang.Types.Type

import scala.collection.mutable

final class LocalVarsCtx private(private val globalVarsCtx: GlobalVarsCtx) {
  private val variables = mutable.Map.empty[String, String]
  
  def currNameFor(rawName: String): Option[String] = {
    variables.get(rawName)
  }
  
  def addAndRetNameFor(rawName: String, tpe: Type): String = {
    globalVarsCtx.addVarRetName(rawName, tpe)
  }
  
  def copied: LocalVarsCtx = {
    val newLvc = new LocalVarsCtx(globalVarsCtx)
    newLvc.variables.addAll(variables)
    newLvc
  }

}

object LocalVarsCtx {
  
  def newInstance(): LocalVarsCtx = new LocalVarsCtx(new GlobalVarsCtx())
  
}
