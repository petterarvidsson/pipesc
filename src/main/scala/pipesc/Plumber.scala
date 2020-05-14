package pipesc

import java.nio.file.Path

case class UnrolledController(controller: Int, statement: NativePipeStatement, inputs: Set[KnobDefinition])
case class UnrolledPipeProgram(controllers: Seq[UnrolledController],
                               knobs: Map[String, KnobDefinition],
                               groups: Map[String, GroupDefinition])

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

object Plumber {
  val MidiBounds = MinMax(0, 127)
}

class Plumber(val path: Path) {

  def functionApplication(fn: FunctionDefinition,
                          unrolledArguments: Seq[(NativePipeStatement, Seq[CompilerError])],
                          functions: Map[NSIdentifier, Seq[Function]]): (NativePipeStatement, Seq[CompilerError]) = {
    unroll(fn, fn.arguments.zip(unrolledArguments).toMap, functions)
  }

  def nativeApplication(
      identifier: NSIdentifier,
      nf: NativeFunction,
      unrolledArguments: Seq[(NativePipeStatement, Seq[CompilerError])]): (NativePipeStatement, Seq[CompilerError]) = {
    (NativeFunctionApplication(identifier, unrolledArguments(0)._1, unrolledArguments(1)._1, nf),
     unrolledArguments(0)._2 ++ unrolledArguments(1)._2)
  }

  def unroll(f: FunctionApplication,
             arguments: Map[String, (NativePipeStatement, Seq[CompilerError])],
             functions: Map[NSIdentifier, Seq[Function]]): (NativePipeStatement, Seq[CompilerError]) = {
    val unrolledArguments = f.arguments.map {
      case f: FunctionApplication =>
        unroll(f, arguments, functions)
      case f: IfStatement =>
        val (c, cErrors) = unroll(f.cond, arguments, functions)
        val (t, tErrors) = unroll(f.`then`, arguments, functions)
        val (e, eErrors) = unroll(f.`else`, arguments, functions)
        (NativeIfStatement(c, t, e), cErrors ++ tErrors ++ eErrors)
      case Value(identifier) =>
        arguments(identifier)
      case c: Constant => (c, Seq.empty)
    }

    val argumentsMinMax = unrolledArguments.map(_._1.minMax)

    Function.find(f.identifier, argumentsMinMax, functions) match {
      case Right(fn: FunctionDefinition) =>
        functionApplication(fn, unrolledArguments, functions)
      case Right(nf: NativeFunction) =>
        nativeApplication(f.identifier, nf, unrolledArguments)
      case Left(Seq()) =>
        (NativePipeStatement.NoopStatement,
         unrolledArguments.map(_._2).flatten :+ CompilerError(path, f.pos, s"No such method ${f.identifier.name}"))
      case Left(alternatives) =>
        val alternativesString = alternatives.map { a =>
          s"  ${Type.signatureString(a.signature)}"
        } mkString ("\n")
        val argumentsString = argumentsMinMax.map(_.prettyPrint).mkString(", ")
        val errorString =
          s"Method ${f.identifier.name} with alternatives:\n$alternativesString\ncannot be applied to ($argumentsString)"
        (NativePipeStatement.NoopStatement,
         unrolledArguments.map(_._2).flatten :+ CompilerError(path, f.pos, errorString))
    }
  }

  def unroll(statement: PipeStatement,
             arguments: Map[String, (NativePipeStatement, Seq[CompilerError])],
             functions: Map[NSIdentifier, Seq[Function]]): (NativePipeStatement, Seq[CompilerError]) = {
    statement match {
      case Value(identifier) => arguments(identifier)
      case f: FunctionApplication =>
        unroll(f, arguments, functions)
      case f: IfStatement =>
        val (c, cErrors) = unroll(f.cond, arguments, functions)
        val (t, tErrors) = unroll(f.`then`, arguments, functions)
        val (e, eErrors) = unroll(f.`else`, arguments, functions)
        (NativeIfStatement(c, t, e), cErrors ++ tErrors ++ eErrors)
      case c: Constant => (c, Seq.empty)
    }
  }

  def unroll(fn: FunctionDefinition,
             arguments: Map[String, (NativePipeStatement, Seq[CompilerError])],
             functions: Map[NSIdentifier, Seq[Function]]): (NativePipeStatement, Seq[CompilerError]) = {
    unroll(fn.statement, arguments, functions)
  }

  def unroll(controller: MidiControllerDefinition,
             arguments: Map[String, (NativePipeStatement, Seq[CompilerError])],
             functions: Map[NSIdentifier, Seq[Function]]): (NativePipeStatement, Seq[CompilerError]) = {
    unroll(controller.statement, arguments, functions)
  }

  def unroll(program: PipeProgram): (UnrolledPipeProgram, Seq[CompilerError]) = {
    val controllers = for (controller <- program.controllers) yield {
      val arguments: Map[String, (KnobDefinition, Seq[CompilerError])] = controller.arguments.map { identifier =>
        (identifier, (program.knobs(identifier), Seq.empty))
      } toMap
      val (statement, errors) = unroll(controller, arguments, program.functions ++ Instruction.nativeFunctions)
      val minMax = statement.minMax
      val controllerErrors = if (!Plumber.MidiBounds.withinBounds(statement.minMax)) {
        Seq(
          CompilerError(
            path,
            controller.statement.pos,
            s"${minMax.prettyPrint} is out of controller #${controller.controller} bounds [${Plumber.MidiBounds.prettyPrint}]"))
      } else {
        Seq.empty
      }
      (UnrolledController(controller.controller, statement, arguments.values.map(_._1).toSet),
       errors ++ controllerErrors)
    }

    (UnrolledPipeProgram(
       controllers.map(_._1),
       program.knobs,
       program.groups
     ),
     controllers.map(_._2).flatten)
  }
}
