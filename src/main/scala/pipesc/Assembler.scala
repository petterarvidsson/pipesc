package pipesc

import scala.math.max

sealed trait Instruction

case class BinaryInstruction(opcode: Int, arg1: Int, arg2: Int, out: Int) extends Instruction
case class UnaryInstruction(opcode: Int, address: Int, out: Int) extends Instruction
case class NullaryInstruction(opcode: Int, constant: Constant, out: Int) extends Instruction

object Instruction {
  // ALU
  val ADD = 0x00
  val SUB = 0x01
  val MUL = 0x02
  val DIV = 0x03
  val MOD = 0x04

  // BRANCH
  val SET = 0x10
  val SETN = 0x11

  // MEMORY
  val LOAD = 0x20
  val CNT = 0x21 // Set memory to constant

  val i2o = Map[NSIdentifier, Int](
    NSIdentifier(Predef.NS, Predef.Add) -> ADD,
    NSIdentifier(Predef.NS, Predef.Sub) -> SUB,
    NSIdentifier(Predef.NS, Predef.Mul) -> MUL,
    NSIdentifier(Predef.NS, Predef.Div) -> DIV,
    NSIdentifier(Predef.NS, Predef.Mod) -> MOD
  )
}

case class Fragment(instructions: Seq[Instruction], maxOffset: Int) {
  def ++(fragment: Fragment) =
    Fragment(instructions ++ fragment.instructions, max(maxOffset, fragment.maxOffset))
}

object Fragment {
  def apply(instruction: Instruction, offset: Int): Fragment =
    Fragment(Seq(instruction), offset)
}

case class Program(instructions: Seq[Instruction], stackSize: Int, knobs: Map[Int, KnobDefinition])

object Program {
  def prettyPrint(key: String, knob: KnobDefinition, indentation: Int) {
    NativePipeStatement.printIndented(s"$key: $knob", indentation)
  }

  def prettyPrint(program: Program) {
    println(s"Stack size: ${program.stackSize}")
    println(s"Knobs:")
    for ((key, knob) <- program.knobs) {
      prettyPrint(key, knob, 2)
    }
    println("Values:")
    for ((k, v) <- program.values) {
      NativePipeStatement.printIndented(s"$k: $v", 2)
    }
    println("Instructions:")
    for (i <- program.instructions) {
      NativePipeStatement.printIndented(i, 2)
    }
  }

}

object Assembler {

  import Instruction._

  def assemble(statement: NativePipeStatement, offset: Int, values: Map[String, Int]): Fragment =
    statement match {
      case NativeFunctionApplication(identifier, arg1, arg2) =>
        val outOffset = offset
        val arg1Offset = offset + 1
        val arg1Fragment = assemble(arg1, arg1Offset, values)
        val arg2Offset = offset + 2
        val arg2Fragment = assemble(arg2, arg2Offset, values)
        arg1Fragment ++ arg2Fragment ++ Fragment(BinaryInstruction(i2o(identifier), arg1Offset, arg2Offset, outOffset),
                                                 arg2Offset)
      case NativeIfStatement(c, t, e) =>
        val outOffset = offset
        val condOffset = offset + 1
        val condFragment = assemble(c, condOffset, values)
        val thenOffset = offset + 2
        val thenFragment = assemble(t, thenOffset, values)
        val elseOffset = offset + 3
        val elseFragment = assemble(e, elseOffset, values)
        condFragment ++ thenFragment ++ elseFragment ++ Fragment(
          Seq(BinaryInstruction(SET, condOffset, thenOffset, outOffset),
              BinaryInstruction(SETN, condOffset, elseOffset, outOffset)),
          elseOffset)
      case constant: Constant =>
        Fragment(NullaryInstruction(CNT, constant, offset), offset)
      case Value(identifier) =>
        Fragment(UnaryInstruction(LOAD, values(identifier), offset), offset)
    }

  def assemble(unrolledProgram: UnrolledPipeProgram): Program = {
    val valueMapping = unrolledProgram.controllers.flatMap(_.inputs.map(_.identifier)).toSet.zipWithIndex.toMap
    val controllers = for ((controller, i) <- unrolledProgram.controllers.zipWithIndex) yield {
      val stackOffset = valueMapping.size + i
      val fragment = assemble(controller.statement, stackOffset, valueMapping)
      (fragment.instructions, fragment.maxOffset + 1)
    }
    Program(controllers.map(_._1).flatten, valueMapping, controllers.map(_._2).max, unrolledProgram.knobs)
  }

}
