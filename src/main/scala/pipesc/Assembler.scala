package pipesc

import scala.math.max

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

  def typeMinMax(f: (MinMax, MinMax) => MinMax)(t1: Type, t2: Type): Type =
    IntegerType(Type.merge(for {
      m1 <- t1.intervals
      m2 <- t2.intervals
    } yield {
      f(m1, m2)
    }))

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
    NSIdentifier(Predef.NS, Predef.Add) -> NativeFunction(Seq(FullIntRange, FullIntRange), typeMinMax(addMinMax)),
    NSIdentifier(Predef.NS, Predef.Sub) -> NativeFunction(Seq(FullIntRange, FullIntRange), typeMinMax(subMinMax)),
    NSIdentifier(Predef.NS, Predef.Mul) -> NativeFunction(Seq(FullIntRange, FullIntRange), typeMinMax(mulMinMax)),
    NSIdentifier(Predef.NS, Predef.Div) -> NativeFunction(Seq(FullIntRange, NonZero), typeMinMax(divMinMax)),
    NSIdentifier(Predef.NS, Predef.Mod) -> NativeFunction(Seq(FullIntRange, NonZero), typeMinMax(modMinMax))
  )

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
                   controllers: Map[Int, InputController],
                   groups: Map[Int, GroupDefinition],
                   ccs: Map[Int, Int])
case class InputController(group: Int, row: Int, column: Int, description: Text, min: Int, max: Int, step: Int)
object InputController {
  def apply(group: Int, controller: ControllerDefinition): InputController =
    apply(group,
          controller.row,
          controller.column,
          controller.description,
          controller.min,
          controller.max,
          controller.step)
}

object Program {
  def prettyPrint(key: Int, controller: InputController, indentation: Int) {
    NativePipeStatement.printIndented(s"$key: $controller", indentation)
  }

  def prettyPrint(program: Program) {
    println(s"Stack size: ${program.stackSize}")
    println(s"Controllers:")
    for ((key, controller) <- program.controllers) {
      prettyPrint(key, controller, 2)
    }
    println("Groups:")
    for ((k, v) <- program.groups) {
      NativePipeStatement.printIndented(s"$k: $v", 2)
    }
    println("Instructions:")
    for (i <- program.instructions) {
      NativePipeStatement.printIndented(i, 2)
    }
    println("CCs:")
    for ((k, v) <- program.ccs) {
      NativePipeStatement.printIndented(s"$k: $v", 2)
    }
  }

}

object Assembler {

  import Instruction._

  def assemble(statement: NativePipeStatement, offset: Int, values: Map[String, Int]): Fragment =
    statement match {
      case NativeFunctionApplication(identifier, arg1, arg2, _) =>
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
      case ControllerDefinition(identifier, _, _, _, _, _, _, _) =>
        Fragment(UnaryInstruction(LOAD, values(identifier), offset), offset)
      case _: Noop =>
        Fragment.empty

    }

  def assemble(unrolledProgram: UnrolledPipeProgram): Program = {
    val valueMapping = unrolledProgram.ccs.flatMap(_.inputs.map(_.identifier)).toSet.zipWithIndex.toMap
    val groupMapping = unrolledProgram.groups.keySet.zipWithIndex.toMap
    val ccs = for ((cc, i) <- unrolledProgram.ccs.zipWithIndex) yield {
      val stackOffset = valueMapping.size + i
      val fragment = assemble(cc.statement, stackOffset, valueMapping)
      (stackOffset, fragment.instructions, fragment.maxOffset + 1, cc.cc)
    }
    val controllers = unrolledProgram.controllers.map {
      case (id, controller) => valueMapping(id) -> InputController(groupMapping(controller.groupIdentifier), controller)
    }
    val groups = unrolledProgram.groups.map {
      case (id, group) => groupMapping(id) -> group
    }
    Program(ccs.map(_._2).flatten, ccs.map(_._3).max, controllers, groups, ccs.map(c => c._1 -> c._4).toMap)
  }

}
