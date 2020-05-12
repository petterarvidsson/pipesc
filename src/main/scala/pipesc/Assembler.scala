package pipesc

import scala.math.max

case class NativeFunction(signature: Seq[Type], minMax: (MinMax, MinMax) => MinMax)

sealed trait Instruction

case class BinaryInstruction(opcode: Int, arg1: Int, arg2: Int, out: Int) extends Instruction
case class UnaryInstruction(opcode: Int, address: Int, out: Int) extends Instruction
case class NullaryInstruction(opcode: Int, constant: Constant, out: Int) extends Instruction

object Instruction {
  val IntMax = 2147483647
  val IntMin = -2147483646

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

  def addMinMax(m1: MinMax, m2: MinMax): MinMax =
    MinMax(m1.min + m2.min, m1.max + m2.max)

  def subMinMax(m1: MinMax, m2: MinMax): MinMax =
    MinMax(m1.min - m2.max, m1.max - m2.min)

  def mulMinMax(m1: MinMax, m2: MinMax): MinMax =
    MinMax(m1.min * m2.min, m1.max * m2.max)

  def divMinMax(m1: MinMax, m2: MinMax): MinMax =
    MinMax(m1.min / m2.max, m1.max / m2.min)

  def modMinMax(m1: MinMax, m2: MinMax): MinMax =
    m2

  val NonZero = IntegerType(Seq(MinMax(IntMin, -1), MinMax(1, IntMax)))
  val FullIntRange = IntegerType(Seq(MinMax(IntMin, IntMax)))

  val nativeFunctions = Map[NSIdentifier, NativeFunction](
    NSIdentifier(Predef.NS, Predef.Add) -> NativeFunction(Seq(FullIntRange, FullIntRange), addMinMax),
    NSIdentifier(Predef.NS, Predef.Sub) -> NativeFunction(Seq(FullIntRange, FullIntRange), subMinMax),
    NSIdentifier(Predef.NS, Predef.Mul) -> NativeFunction(Seq(FullIntRange, FullIntRange), mulMinMax),
    NSIdentifier(Predef.NS, Predef.Div) -> NativeFunction(Seq(FullIntRange, NonZero), divMinMax),
    NSIdentifier(Predef.NS, Predef.Mod) -> NativeFunction(Seq(FullIntRange, NonZero), modMinMax)
  )

  def findNativeFunction(identifier: NSIdentifier, args: Seq[MinMax]): (Option[NativeFunction], Seq[NativeFunction]) = {
    nativeFunctions.get(identifier) match {
      case Some(nf) =>
        if(args.size == nf.signature.size) {
          if(args.zip(nf.signature).foldLeft(true){ (acc, e) =>
            acc & Plumber.withinBounds(e._1, e._2.intervals)
          }) {
            (Some(nf), Seq(nf))
          } else {
            (None, Seq(nf))
          }
        } else {
          (None, Seq(nf))
        }

      case None =>
        (None, Seq.empty)
    }
  }
}

case class Fragment(instructions: Seq[Instruction], maxOffset: Int) {
  def ++(fragment: Fragment) =
    Fragment(instructions ++ fragment.instructions, max(maxOffset, fragment.maxOffset))
}

object Fragment {
  def apply(instruction: Instruction, offset: Int): Fragment =
    Fragment(Seq(instruction), offset)
  val empty = apply(Seq.empty, 0)
}

case class Program(instructions: Seq[Instruction],
                   stackSize: Int,
                   knobs: Map[Int, InputKnob],
                   groups: Map[Int, GroupDefinition],
                   controllers: Map[Int, Int])
case class InputKnob(group: Int, row: Int, column: Int, description: Text, min: Int, max: Int, step: Int)
object InputKnob {
  def apply(group: Int, knob: KnobDefinition): InputKnob =
    apply(group, knob.row, knob.column, knob.description, knob.min, knob.max, knob.step)
}

object Program {
  def prettyPrint(key: Int, knob: InputKnob, indentation: Int) {
    NativePipeStatement.printIndented(s"$key: $knob", indentation)
  }

  def prettyPrint(program: Program) {
    println(s"Stack size: ${program.stackSize}")
    println(s"Knobs:")
    for ((key, knob) <- program.knobs) {
      prettyPrint(key, knob, 2)
    }
    println("Groups:")
    for ((k, v) <- program.groups) {
      NativePipeStatement.printIndented(s"$k: $v", 2)
    }
    println("Instructions:")
    for (i <- program.instructions) {
      NativePipeStatement.printIndented(i, 2)
    }
    println("Controllers:")
    for ((k, v) <- program.controllers) {
      NativePipeStatement.printIndented(s"$k: $v", 2)
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
      case KnobDefinition(identifier, _, _, _, _, _, _, _) =>
        Fragment(UnaryInstruction(LOAD, values(identifier), offset), offset)
      case _: Noop =>
        Fragment.empty

    }

  def assemble(unrolledProgram: UnrolledPipeProgram): Program = {
    val valueMapping = unrolledProgram.controllers.flatMap(_.inputs.map(_.identifier)).toSet.zipWithIndex.toMap
    val groupMapping = unrolledProgram.groups.keySet.zipWithIndex.toMap
    val controllers = for ((controller, i) <- unrolledProgram.controllers.zipWithIndex) yield {
      val stackOffset = valueMapping.size + i
      val fragment = assemble(controller.statement, stackOffset, valueMapping)
      (stackOffset, fragment.instructions, fragment.maxOffset + 1, controller.controller)
    }
    val knobs = unrolledProgram.knobs.map {
      case (id, knob) => valueMapping(id) -> InputKnob(groupMapping(knob.groupIdentifier), knob)
    }
    val groups = unrolledProgram.groups.map {
      case (id, group) => groupMapping(id) -> group
    }
    Program(controllers.map(_._2).flatten,
            controllers.map(_._3).max,
            knobs,
            groups,
            controllers.map(c => c._1 -> c._4).toMap)
  }

}
