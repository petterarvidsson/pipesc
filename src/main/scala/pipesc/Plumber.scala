package pipesc

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

  def unroll(entryPoint: Identifier, functions: Map[Identifier, FunctionDefinition]): NativePipeStatement = {
    val entry = functions(entryPoint)
    require(entry.arguments.size == 0, s"Entry point: ${entryPoint.name} must have 0 arguments")
    unroll(FunctionApplication(entry.identifier, Seq()), Map[Identifier, NativePipeStatement](), functions)
  }
}
