package pipesc

import java.nio.{ByteBuffer, ByteOrder}

object Binary {

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

  private def binaryEncodeProgram(buffer: ByteBuffer, program: Program): ByteBuffer =
    binaryEncodeInstructions(buffer, program.instructions)
      .putShort(math.ceil(math.log(program.stackSize) / math.log(2)).toShort)

  def binaryEncode(program: Program): ByteBuffer =
    binaryEncodeProgram(ByteBuffer
      .allocate(512)
      .order(ByteOrder.BIG_ENDIAN),
      program
    )

}
