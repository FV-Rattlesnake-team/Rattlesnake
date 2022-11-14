package compiler

import compiler.AnalysisContext
import compiler.CompilationStep.ContextCreation
import compiler.Errors.{CompilationError, Err, ErrorReporter, errorsExitCode}
import compiler.irs.Asts.{FunDef, StructDef}
import lang.Types.PrimitiveType.{NothingType, VoidType}
import lang.Types.Type
import lang.{BuiltInFunctions, FunctionSignature, StructSignature, Types}

import scala.annotation.tailrec
import scala.collection.mutable

final case class AnalysisContext(functions: Map[String, FunctionSignature], structs: Map[String, StructSignature]){
  
  @tailrec def knowsType(tpe: Type): Boolean = {
    tpe match {
      case _: Types.PrimitiveType => true
      case Types.StructType(typeName) => structs.contains(typeName)
      case Types.ArrayType(elemType) => knowsType(elemType)
      case Types.UndefinedType => true
    }
  }
  
}

object AnalysisContext {

  final class Builder(errorReporter: ErrorReporter) {
    private val functions: mutable.Map[String, FunctionSignature] = mutable.Map.empty
    private val structs: mutable.Map[String, StructSignature] = mutable.Map.empty

    def addFunction(funDef: FunDef): Unit = {
      val name = funDef.funName
      if (BuiltInFunctions.builtInFunctions.contains(name)) {
        errorReporter.push(Err(ContextCreation, s"function name '$name' conflicts with built-in function", funDef.getPosition))
      } else if (functions.contains(name)) {
        errorReporter.push(Err(ContextCreation, s"redefinition of function '$name'", funDef.getPosition))
      } else {
        functions.put(name, funDef.signature)
      }
    }

    def addStruct(structDef: StructDef): Unit = {
      val name = structDef.structName
      if (structs.contains(name)) {
        errorReporter.push(Err(ContextCreation, s"redefinition of struct '$name'", structDef.getPosition))
      } else {
        val fieldsMap = new mutable.LinkedHashMap[String, Type]()
        for param <- structDef.fields do {
          if (param.tpe == VoidType || param.tpe == NothingType) {
            errorReporter.push(Err(ContextCreation, s"struct field cannot have type '${param.tpe}'", param.getPosition))
          } else if (fieldsMap.contains(param.paramName)) {
            errorReporter.push(Err(ContextCreation, s"duplicated field: '${param.paramName}'", param.getPosition))
          } else {
            fieldsMap.put(param.paramName, param.tpe)
          }
        }
        val sig = StructSignature(name, fieldsMap)
        structs.put(name, sig)
      }
    }

    def build(): AnalysisContext = {
      functions.addAll(BuiltInFunctions.builtInFunctions)
      new AnalysisContext(functions.toMap, structs.toMap)
    }

  }

}
