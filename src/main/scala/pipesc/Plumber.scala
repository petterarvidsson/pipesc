package pipesc

case class UnrolledController(controller: Int, statement: NativePipeStatement, inputs: Set[Value])
case class UnrolledPipeProgram(controllers: Seq[UnrolledController], knobs: Map[String, KnobDefinition])

object UnrolledPipeProgram {
  def prettyPrint(program: UnrolledPipeProgram) {
    println("Knobs:")
    for (knob <- program.knobs) {
      println(knob)
    }
    for (controller <- program.controllers) {
      val inputs = controller.inputs.map(_.identifier).mkString(", ")
      NativePipeStatement.printIndented(s"CC${controller.controller}($inputs) {", 0)
      NativePipeStatement.prettyPrint(controller.statement, 1)
      NativePipeStatement.printIndented(s"}", 0)
    }
  }
}

class Plumber {

  def convertArguments(fn: FunctionDefinition,
                       functionArguments: Seq[PipeStatement],
                       arguments: Map[String, NativePipeStatement],
                       functions: Map[NSIdentifier, FunctionDefinition]): Map[String, NativePipeStatement] =
    fn.arguments.zip(functionArguments) map {
      case (identifier, Value(oldIdentifier)) =>
        identifier -> arguments(oldIdentifier)
      case (identifier, statement) =>
        identifier -> unroll(statement, arguments, functions)
    } toMap

  def unroll(f: FunctionApplication,
             arguments: Map[String, NativePipeStatement],
             functions: Map[NSIdentifier, FunctionDefinition]): NativePipeStatement = {
    val unrolledArguments = f.arguments.map {
      case f: FunctionApplication =>
        unroll(f, arguments, functions)
      case f: IfStatement =>
        NativeIfStatement(unroll(f.cond, arguments, functions),
                          unroll(f.`then`, arguments, functions),
                          unroll(f.`else`, arguments, functions))
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
             arguments: Map[String, NativePipeStatement],
             functions: Map[NSIdentifier, FunctionDefinition]): NativePipeStatement = {
    statement match {
      case Value(identifier) => arguments(identifier)
      case f: FunctionApplication =>
        unroll(f, arguments, functions)
      case f: IfStatement =>
        NativeIfStatement(unroll(f.cond, arguments, functions),
                          unroll(f.`then`, arguments, functions),
                          unroll(f.`else`, arguments, functions))
      case c: Constant => c
    }
  }

  def unroll(fn: FunctionDefinition,
             arguments: Map[String, NativePipeStatement],
             functions: Map[NSIdentifier, FunctionDefinition]): NativePipeStatement = {
    unroll(fn.statement, arguments, functions)
  }

  def unroll(controller: MidiControllerDefinition,
             arguments: Map[String, NativePipeStatement],
             functions: Map[NSIdentifier, FunctionDefinition]): NativePipeStatement = {
    unroll(controller.statement, arguments, functions)
  }

  def unroll(program: PipeProgram): UnrolledPipeProgram = {
    UnrolledPipeProgram(
      for (controller <- program.controllers) yield {
        val arguments = controller.arguments.map { identifier =>
          (identifier, Value(identifier))
        } toMap
        val statement = unroll(controller, arguments, program.functions)
        UnrolledController(controller.controller, statement, arguments.values.toSet)
      },
      program.knobs
    )
  }
}
