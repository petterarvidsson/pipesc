package pipesc

case class EntryPoint(output: NativePipeStatement, inputs: Set[Value])

object EntryPoint {
  def prettyPrint(entryPoint: EntryPoint) {
    val inputs = entryPoint.inputs.map(_.identifier.name).mkString(", ")
    NativePipeStatement.printIndented(s"entry($inputs) {", 0)
    NativePipeStatement.prettyPrint(entryPoint.output, 1)
    NativePipeStatement.printIndented(s"}", 0)
  }
}

class Plumber {

  def convertArguments(fn: FunctionDefinition,
                       functionArguments: Seq[PipeStatement],
                       arguments: Map[Identifier, NativePipeStatement],
                       functions: Map[Identifier, FunctionDefinition]): Map[Identifier, NativePipeStatement] =
    fn.arguments.zip(functionArguments) map {
      case (identifier, Value(oldIdentifier)) =>
        identifier -> arguments(oldIdentifier)
      case (identifier, statement) =>
        identifier -> unroll(statement, arguments, functions)
    } toMap

  def unroll(f: FunctionApplication,
             arguments: Map[Identifier, NativePipeStatement],
             functions: Map[Identifier, FunctionDefinition]): NativePipeStatement = {
    val unrolledArguments = f.arguments.map {
      case f: FunctionApplication =>
        unroll(f, arguments, functions)
      case Value(identifier) =>
        arguments(identifier)
      case c: Constant => c
    }
    functions.get(f.identifier) match {
      case Some(fn) =>
        unroll(fn, convertArguments(fn, f.arguments, arguments, functions), functions)
      case None =>
        NativeFunctionApplication(f.identifier, unrolledArguments(0), unrolledArguments(1))
    }
  }

  def unroll(statement: PipeStatement,
             arguments: Map[Identifier, NativePipeStatement],
             functions: Map[Identifier, FunctionDefinition]): NativePipeStatement = {
    statement match {
      case Value(identifier) => arguments(identifier)
      case f: FunctionApplication =>
        unroll(f, arguments, functions)
      case c: Constant => c
    }
  }

  def unroll(fn: FunctionDefinition,
             arguments: Map[Identifier, NativePipeStatement],
             functions: Map[Identifier, FunctionDefinition]): NativePipeStatement = {
    unroll(fn.statement, arguments, functions)
  }

  def unroll(entryPoint: Identifier, functions: Map[Identifier, FunctionDefinition]): EntryPoint = {
    val entry = functions(entryPoint)
    val arguments =  entry.arguments.map { identifier =>
      (identifier,  Value(identifier))
    } toMap

    //require(entry.arguments.size == 0, s"Entry point: ${entryPoint.name} must have 0 arguments")
    EntryPoint(unroll(entry, arguments, functions), arguments.values.toSet)
  }
}
