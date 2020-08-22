package pipesc

import java.nio.{ByteBuffer, ByteOrder}

/**
  * Format:
  * | 16 bits: <format version> 0 |
  * | 32 bits: <number of instructions>|
  * x | 4 x 16 bits: <instruction> |
  * | 8 bits: <stack size> |
  * | 8 bits: <number of groups> |
  * x | 4 x 8 bits: <row from> <column from> <row to> <column to> |
  * x | 8 bits: <description length>
  * x x | <description> |
  * | 8 bits: <number of controllers> |
  * x | 8 bits: <group>
  * x | 2 x 8 bits: <row> <column> |
  * x | 2 x 32 bits: <min> <max> |
  * x | 32 bits: <step> |
  * x | 8 bits: <description length>
  * x x | <description> |
  * | 16 bits: <number of ccs> |
  * x | 16 bits: <memory offset> |
  * x | 16 bits: <CC#> |
  */
object Binary {
  val CurrentVersion = 0.toShort

  private def binaryEncode(buffer: ByteBuffer, instruction: Instruction): ByteBuffer =
    instruction match {
      case BinaryInstruction(opcode, arg1, arg2, out) =>
        buffer
          .putShort(opcode.toShort)
          .putShort(arg1.toShort)
          .putShort(arg2.toShort)
          .putShort(out.toShort)
      case UnaryInstruction(opcode, address, out) =>
        buffer
          .putShort(opcode.toShort)
          .putShort(address.toShort)
          .putShort(0)
          .putShort(out.toShort)
      case NullaryInstruction(opcode, Constant(value), out) =>
        buffer
          .putShort(opcode.toShort)
          .putInt(value)
          .putShort(out.toShort)
    }

  private def binaryEncodeInstructions(buffer: ByteBuffer, instructions: Seq[Instruction]): ByteBuffer =
    instructions.foldLeft(buffer.putInt(instructions.size))(binaryEncode(_, _))

  private def binaryEncode(buffer: ByteBuffer, group: GroupDefinition): ByteBuffer =
    buffer
      .put(group.rowFrom.toByte)
      .put(group.columnFrom.toByte)
      .put(group.rowTo.toByte)
      .put(group.columnTo.toByte)
      .put(group.description.text.length.toByte)
      .put(group.description.text.getBytes("UTF-8"))

  private def binaryEncodeGroups(buffer: ByteBuffer, groups: Seq[GroupDefinition]): ByteBuffer =
    groups.foldLeft(buffer.put(groups.size.toByte))(binaryEncode(_, _))

  private def binaryEncode(buffer: ByteBuffer, controller: InputController): ByteBuffer =
    buffer
      .put(controller.group.toByte)
      .put(controller.row.toByte)
      .put(controller.column.toByte)
      .putInt(controller.min)
      .putInt(controller.max)
      .putInt(controller.step)
      .put(controller.description.text.length.toByte)
      .put(controller.description.text.getBytes("UTF-8"))

  private def binaryEncodeControllers(buffer: ByteBuffer, controllers: Seq[InputController]): ByteBuffer =
    controllers.foldLeft(buffer.put(controllers.size.toByte))(binaryEncode(_, _))

  private def binaryEncode(buffer: ByteBuffer, cc: (Int, Int)): ByteBuffer =
    buffer
      .putShort(cc._1.toShort)
      .putShort(cc._2.toShort)

  private def binaryEncodeCcs(buffer: ByteBuffer, ccs: Seq[(Int, Int)]): ByteBuffer =
    ccs.foldLeft(buffer.putShort(ccs.size.toShort))(binaryEncode(_, _))

  private def binaryEncodeProgram(buffer: ByteBuffer, program: Program): ByteBuffer = {
    binaryEncodeCcs(
      binaryEncodeControllers(
        binaryEncodeGroups(
          binaryEncodeInstructions(buffer.putShort(CurrentVersion), program.instructions)
            .put(math.ceil(math.log(program.stackSize) / math.log(2)).toByte),
          program.groups.toSeq.sortBy(_._1).map(_._2)
        ),
        program.controllers.toSeq.sortBy(_._1).map(_._2)
      ),
      program.ccs.toSeq
    )
  }
  def binaryEncode(program: Program): ByteBuffer =
    binaryEncodeProgram(ByteBuffer
                          .allocate(1024)
                          .order(ByteOrder.BIG_ENDIAN),
                        program)

}
