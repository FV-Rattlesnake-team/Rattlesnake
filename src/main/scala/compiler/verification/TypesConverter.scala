package compiler.verification

import lang.Types
import lang.Types.{PrimitiveType, Type}
import smtlib.theories.Core.BoolSort
import smtlib.theories.Ints.IntSort
import smtlib.theories.Reals.RealSort
import smtlib.theories.experimental.Strings.StringSort
import smtlib.trees.Terms.Sort

object TypesConverter {

  def convert(tpe: Type): Sort = {
    tpe match
      case PrimitiveType.IntType => IntSort()
      case PrimitiveType.DoubleType => RealSort()
      case PrimitiveType.CharType => IntSort()
      case PrimitiveType.BoolType => BoolSort()
      case PrimitiveType.StringType => StringSort()
      case Types.StructType(typeName) => ???  // TODO
      case Types.ArrayType(elemType) => ??? // TODO
      case Types.UndefinedType => assert(false)
      case PrimitiveType.VoidType => assert(false)
      case PrimitiveType.NothingType => assert(false)
  }

}
