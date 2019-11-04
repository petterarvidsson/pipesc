package pipesc

import java.nio.{ByteBuffer, ByteOrder}

/**
  * Format:
  * | 16 bits: <format version> 0 |
  * | 16 bits: <number of instructions>|
  * x | 4 x 16 bits: <instruction> |
  * | 16 bits: <stack size>|
  * | 16 bits: <number of groups> |
  * x | 4 x 8 bits: <row from> <column from> <row to> <column to> |
  * x | 8 bits: <description length>
  * x x | <description> |
  * | 16 bits: <number of knobs> |
  * x | 2 x 8 bits: <row> <column> |
  * x | 8 bits: <description length>
  * x x | <description> |
  * | 16 bits: <number of controllers> |
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
      case NullaryInstruction(opcode, Constant(IntNum(value)), out) =>
        buffer
          .putShort(opcode.toShort)
          .putShort(value.toShort)
          .putShort(0)
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
    groups.foldLeft(buffer.putShort(groups.size.toShort))(binaryEncode(_ ,_))

  private def binaryEncode(buffer: ByteBuffer, knob: InputKnob): ByteBuffer =
    buffer
      .put(knob.row.toByte)
      .put(knob.column.toByte)
      .put(knob.description.text.length.toByte)
      .put(knob.description.text.getBytes("UTF-8"))

  private def binaryEncodeKnobs(buffer: ByteBuffer, knobs: Seq[InputKnob]): ByteBuffer =
    knobs.foldLeft(buffer.putShort(knobs.size.toShort))(binaryEncode(_ ,_))

  private def binaryEncode(buffer: ByteBuffer, cc: (Int, Int)): ByteBuffer =
    buffer
      .putShort(cc._1.toShort)
      .putShort(cc._2.toShort)

  private def binaryEncodeControllers(buffer: ByteBuffer, controllers: Seq[(Int, Int)]): ByteBuffer =
    controllers.foldLeft(buffer.putShort(controllers.size.toShort))(binaryEncode(_ ,_))

  private def binaryEncodeProgram(buffer: ByteBuffer, program: Program): ByteBuffer = {
    binaryEncodeControllers(
      binaryEncodeKnobs(
        binaryEncodeGroups(
          binaryEncodeInstructions(buffer.putShort(CurrentVersion), program.instructions)
            .putShort(math.ceil(math.log(program.stackSize) / math.log(2)).toShort),
          program.groups.toSeq.sortBy(_._1).map(_._2)
        ),
        program.knobs.toSeq.sortBy(_._1).map(_._2)
      ),
      program.controllers.toSeq
    )
  }
  def binaryEncode(program: Program): ByteBuffer =
    binaryEncodeProgram(ByteBuffer
                          .allocate(512)
                          .order(ByteOrder.BIG_ENDIAN),
                        program)

}
