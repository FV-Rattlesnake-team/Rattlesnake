package compiler.verification

import scala.collection.mutable

final class VarsCtx {
  private val variables = mutable.Map.empty[String, Int]
  
  def newNameFor(rawName: String): String = {
    val oldIdx = variables.getOrElse(rawName, -1)
    val newIdx = oldIdx + 1
    variables(rawName) = newIdx
    makeName(rawName, newIdx)
  }
  
  def nameFor(rawName: String): String = {
    variables.get(rawName) match
      case Some(idx) =>
        makeName(rawName, idx)
      case None =>
        newNameFor(rawName)
  }
  
  private def makeName(rawName: String, idx: Int): String = {
    if idx == 0 then rawName
    else rawName ++ "%" ++ idx.toString
  }
  
}
