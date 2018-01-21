package pipesc

class Plumber {
  import Plumber._
  var higest = 0

  // def nativeFunctionToInstruction(fa: FunctionApplication, out: Int): Instruction = {
  //   require(fa.arguments.size != 2, s"Native function ${fa.identifier.name} takes exactly two arguments")
  //   Instruction(fa.identifier, out)
  // }

  // def unroll(constant: Constant, out: Int): Seq[NativeCode] =

  // def unroll(value: Value, out: Int, arguments: Map[Identifier, Int]): Seq[NativeCode] = {

  // }

  // def unroll(d: Definition, out: Int, arguments: Map[Identifier, Int]): Seq[NativeCode] = {
  //   d.statment match {
  //     case c: Constant =>
  //       unroll(c, out)
  //     case v: Value =>
  //       unroll(v, out, arguments)
  //   }
  // }

  // def unroll(fn: FunctionDefinition, args: Seq[Int]): NaticeCode = {
  //   fn.definitions map {
  //     case Definition(indetifier, Constant(i)) =>
  //       MemoryConstant(out, constant.value.value)
  //   }
  // }

  def convertArguments(fn: FunctionDefinition,
                       functionArguments: Seq[PipeStatement],
                       arguments: Map[Identifier, PipeStatement]): Map[Identifier, PipeStatement] =
    fn.arguments.zip(functionArguments) map {
      case (identifier, Value(oldIdentifier)) =>
        identifier -> arguments(oldIdentifier)
      case (identifier, stmnt) =>
        identifier -> stmnt
    } toMap

  def unroll(f: FunctionApplication,
             arguments: Map[Identifier, PipeStatement],
             functions: Map[Identifier, FunctionDefinition]): PipeStatement = {
    val unrolledArguments = f.arguments.map {
      case f: FunctionApplication =>
        unroll(f, arguments, functions)
      case Value(identifier) => arguments(identifier)
      case stmnt             => stmnt
    }
    functions.get(f.identifier) match {
      case Some(fn) =>
        unroll(fn, convertArguments(fn, f.arguments, arguments), functions)
      case None =>
        f.copy(f.identifier, unrolledArguments)
    }
  }

  def unroll(statement: PipeStatement,
             arguments: Map[Identifier, PipeStatement],
             functions: Map[Identifier, FunctionDefinition]): PipeStatement = {
    statement match {
      case Value(identifier) => arguments(identifier)
      case f: FunctionApplication =>
        unroll(f, arguments, functions)
      case stmnt => stmnt
    }
  }

  def unroll(fn: FunctionDefinition,
             arguments: Map[Identifier, PipeStatement],
             functions: Map[Identifier, FunctionDefinition]): PipeStatement = {
    unroll(fn.statement, arguments, functions)
  }

  def unroll(entryPoint: Identifier, functions: Map[Identifier, FunctionDefinition]): PipeStatement = {
    val entry = functions(entryPoint)
    require(entry.arguments.size == 0, s"Entry point: ${entryPoint.name} must have 0 arguments")
    unroll(FunctionApplication(entry.identifier, Seq()), Map[Identifier, PipeStatement](), functions)
  }
}

object Plumber {
  // sealed trait NativeCode

  // case class MemoryConstant(at: Int, constant: Constant) extends NativeCode
  // case class MemoryFunction(identifier: Identifier, arguments: Seq[Int]) extends NativeCode
  // case class Instruction(identifier: Identifier, in1: Int, in2: Int, out: Int)

  // val nativeFunctions = Set(
  //   "add",
  //   "sub",
  //   "mul",
  //   "div"
  // )

}
