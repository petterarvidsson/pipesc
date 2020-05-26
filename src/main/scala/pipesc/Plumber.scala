package pipesc

import java.nio.file.Path

case class UnrolledCC(cc: Int, statement: NativePipeStatement, inputs: Set[ControllerDefinition])
case class UnrolledPipeProgram(ccs: Seq[UnrolledCC],
                               controllers: Map[String, ControllerDefinition],
                               groups: Map[String, GroupDefinition])

object UnrolledPipeProgram {
  def prettyPrint(program: UnrolledPipeProgram) {
    println("Controllers:")
    for (controller <- program.controllers) {
      println(controller)
    }
    for (cc <- program.ccs) {
      val inputs = cc.inputs.map(_.identifier).mkString(", ")
      NativePipeStatement.printIndented(s"CC${cc.cc}($inputs) {", 0)
      NativePipeStatement.prettyPrint(cc.statement, 1)
      NativePipeStatement.printIndented(s"}", 0)
    }
  }
}

object Plumber {
  val MidiBounds = MinMax(0, 127)
  val MidiType = IntegerType(Seq(MidiBounds))
  val BoolBounds = MinMax(0, 1)
  val BoolType = IntegerType(Seq(BoolBounds))

  def nativeApplication(
      identifier: NSIdentifier,
      nf: NativeFunction,
      unrolledArguments: Seq[(NativePipeStatement, Seq[CompilerError])]): (NativePipeStatement, Seq[CompilerError]) = {
    (NativeFunctionApplication(identifier, unrolledArguments(0)._1, unrolledArguments(1)._1, nf),
     unrolledArguments(0)._2 ++ unrolledArguments(1)._2)
  }

  def unroll(f: IfStatement,
             arguments: Map[String, (NativePipeStatement, Seq[CompilerError])],
             functions: Map[NSIdentifier, Function],
             expectedType: Type): (NativePipeStatement, Seq[CompilerError]) = {
    val (c, cErrors) = unroll(f.cond, arguments, functions, Plumber.BoolType)
    val (t, tErrors) = unroll(f.`then`, arguments, functions, expectedType)
    val (e, eErrors) = unroll(f.`else`, arguments, functions, expectedType)
    (NativeIfStatement(c, t, e), cErrors ++ tErrors ++ eErrors)
  }

  def unroll(a: FunctionApplication,
             arguments: Map[String, (NativePipeStatement, Seq[CompilerError])],
             functions: Map[NSIdentifier, Function],
             expectedType: Type): (NativePipeStatement, Seq[CompilerError]) = {
    functions.get(a.identifier) match {
      case Some(f) if a.arguments.size == f.signature.size =>
        val unrolledArguments = a.arguments.zip(f.signature).map {
          case (s, t) =>
            unroll(s, arguments, functions, t)
        }

        val argumentsMinMax = unrolledArguments.map(_._1.minMax)

        if (f.canBeAppliedTo(argumentsMinMax)) {
          f match {
            case fn: FunctionDefinition =>
              unroll(fn, fn.arguments.zip(unrolledArguments).toMap, functions, expectedType)
            case nf: NativeFunction =>
              nativeApplication(a.identifier, nf, unrolledArguments)
            case Scale =>
              val (statement, errors) = unrolledArguments(0)
              if (expectedType.intervals.size == 1) {
                val minMax = expectedType.intervals.head
                val factor = statement.minMax.magnitude / minMax.magnitude + 1
                (NativeFunctionApplication(
                   NSIdentifier(Predef.NS, Predef.Div),
                   NativeFunctionApplication(NSIdentifier(Predef.NS, Predef.Add),
                                             statement,
                                             Constant(statement.minMax.offset),
                                             Instruction.nativeFunctions(NSIdentifier(Predef.NS, Predef.Add))),
                   Constant(factor),
                   Instruction.nativeFunctions(NSIdentifier(Predef.NS, Predef.Div))
                 ),
                 errors)
              } else {
                (statement,
                 errors :+ CompilerError(a.pos,
                                         s"Scale can not scale to a non-continuous type ${expectedType.prettyPrint}"))
              }
          }
        } else {
          val argumentsString = argumentsMinMax.map(_.prettyPrint).mkString(", ")
          val errorString =
            s"Method ${a.identifier.name} with signature:\n  ${Type.signatureString(f.signature)}\ncannot be applied to: ($argumentsString)"
          (NativePipeStatement.NoopStatement, unrolledArguments.map(_._2).flatten :+ CompilerError(a.pos, errorString))
        }
      case None =>
        (NativePipeStatement.NoopStatement, Seq(CompilerError(a.pos, s"No such method ${a.identifier.name}")))
    }
  }

  def unroll(statement: PipeStatement,
             arguments: Map[String, (NativePipeStatement, Seq[CompilerError])],
             functions: Map[NSIdentifier, Function],
             expectedType: Type): (NativePipeStatement, Seq[CompilerError]) = {
    statement match {
      case Value(identifier) =>
        arguments(identifier)
      case f: FunctionApplication =>
        unroll(f, arguments, functions, expectedType)
      case f: IfStatement =>
        unroll(f, arguments, functions, expectedType)
      case c: Constant =>
        (c, Seq.empty)
    }
  }

  def unroll(fn: FunctionDefinition,
             arguments: Map[String, (NativePipeStatement, Seq[CompilerError])],
             functions: Map[NSIdentifier, Function],
             expectedType: Type): (NativePipeStatement, Seq[CompilerError]) = {
    unroll(fn.statement, arguments, functions, expectedType)
  }

  def unroll(cc: MidiCCDefinition,
             arguments: Map[String, (NativePipeStatement, Seq[CompilerError])],
             functions: Map[NSIdentifier, Function]): (NativePipeStatement, Seq[CompilerError]) = {
    unroll(cc.statement, arguments, functions, Plumber.MidiType)
  }

  def parseAst(program: PipeProgram): Either[Seq[CompilerError], UnrolledPipeProgram] = {

    val ccs = (for (cc <- program.ccs) yield {
      val arguments: Map[String, (ControllerDefinition, Seq[CompilerError])] = cc.arguments.map { identifier =>
        (identifier, (program.controllers(identifier), Seq.empty))
      } toMap
      val (statement, errors) = unroll(
        cc,
        arguments,
        (program.functions ++ Instruction.nativeFunctions) + (NSIdentifier(Predef.NS, Predef.Scale) -> Scale))
      val minMax = statement.minMax
      val ccErrors = if (!Plumber.MidiBounds.withinBounds(statement.minMax)) {
        Seq(
          CompilerError(cc.statement.pos,
                        s"${minMax.prettyPrint} is out of cc #${cc.cc} bounds [${Plumber.MidiBounds.prettyPrint}]"))
      } else {
        Seq.empty
      }
      (UnrolledCC(cc.cc, statement, arguments.values.map(_._1).toSet), errors ++ ccErrors)
    })

    val unrolledProgram = UnrolledPipeProgram(
      ccs.map(_._1),
      program.controllers,
      program.groups
    )

    val errors =
      ccs.map(_._2).flatten

    if (errors.isEmpty) {
      Right(unrolledProgram)
    } else {
      Left(errors)
    }

  }
}
