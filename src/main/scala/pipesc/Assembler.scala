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

case class Program(instructions: Seq[Instruction], values: Map[Identifier, Int], stackSize: Int)
object Program {
  def prettyPrint(program: Program) {
    println(s"Stack size: ${program.stackSize}")
    for(v <- program.values) {
      println(v)
    }
    for(i <- program.instructions) {
      println(i)
    }
  }

}

object Assembler {

  import Instruction._

  def assemble(statement: NativePipeStatement, offset: Int, values: Map[Identifier, Int]): Fragment =
    statement match {
      case NativeFunctionApplication(identifier, arg1, arg2) =>
        val outOffset = offset
        val arg1Offset = offset + 1
        val arg1Fragment = assemble(arg1, arg1Offset, values)
        val arg2Offset = offset + 2
        val arg2Fragment = assemble(arg2, arg2Offset, values)
          arg1Fragment ++ arg2Fragment ++ Fragment(BinaryInstruction(i2o(identifier), arg1Offset, arg2Offset, outOffset), arg2Offset)
      case constant: Constant =>
        Fragment(NullaryInstruction(CNT, constant, offset), offset)
      case Value(identifier) =>
        Fragment(UnaryInstruction(LOAD, values(identifier), offset), offset)
    }

  def assemble(entry: EntryPoint): Program = {
    val stackOffset = entry.inputs.size
    val valueMapping = entry.inputs.map(_.identifier).zipWithIndex.toMap
    val fragment = assemble(entry.output, stackOffset, valueMapping)
    Program(fragment.instructions, valueMapping, fragment.maxOffset + 1)
  }

}
