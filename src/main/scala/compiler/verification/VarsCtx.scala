package compiler.verification

import compiler.irs.Asts.VariableRef

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
  
  def currentRenameMapSnapshot: Map[String, VariableRef] = {
    variables.toMap
      .map((rawName, idx) => (rawName, VariableRef(makeName(rawName, idx))))
  }

  def currRenameMapView: Map[String, VariableRef] = {
    /*
     * Return a custom view so that the variables encountered during the traversal of 
     * the tree being renamed are added to this VarsCtx
     */
    new Map[String, VariableRef]{
      override def removed(key: String): Map[String, VariableRef] = {
        throw new UnsupportedOperationException()
      }

      override def updated[V1 >: VariableRef](key: String, value: V1): Map[String, V1] = {
        throw new UnsupportedOperationException()
      }

      override def get(key: String): Option[VariableRef] = {
        Some(VariableRef(nameFor(key)))
      }

      override def iterator: Iterator[(String, VariableRef)] = {
        throw new UnsupportedOperationException()
      }
    }
  }
  
  private def makeName(rawName: String, idx: Int): String = {
    if idx == 0 then rawName
    else rawName ++ "%" ++ idx.toString
  }
  
}
