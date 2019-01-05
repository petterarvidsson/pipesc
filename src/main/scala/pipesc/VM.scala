package pipesc

import java.nio.{ByteBuffer, ByteOrder}

import Instruction._

object VM {
  def run(program: Program, args: (String, Int)*): Array[Int] = {
    val memory = Array.ofDim[Int](program.stackSize)
    for(arg <- args) {
      memory.update(program.values(Identifier(arg._1)), arg._2)
    }
    for (i <- program.instructions) {
      i match {
        case BinaryInstruction(ADD, arg1, arg2, out) => memory.update(out, memory(arg1) + memory(arg2))
        case BinaryInstruction(MUL, arg1, arg2, out) => memory.update(out, memory(arg1) * memory(arg2))
        case UnaryInstruction(LOAD, address, out) => memory.update(out, memory(address))
        case NullaryInstruction(CNT, constant, out) => memory.update(out, constant.value.value)
      }
    }
    memory
  }

  def run(program: Array[Byte], stackSize: Int, args: Array[Short]): Array[Short] = {
    val memory = Array.ofDim[Short](math.pow(2, stackSize).toInt)
    for(i <- 0 until args.size) {
      memory.update(i, args(i))
    }
    val buffer = ByteBuffer.wrap(program).order(ByteOrder.BIG_ENDIAN)
    for (i <- 0 until (program.size / 8)) {
      val opcode = buffer.getShort()
      val arg1 = buffer.getShort()
      val arg2 = buffer.getShort()
      val out = buffer.getShort()
      opcode match {
        case ADD => memory.update(out, (memory(arg1) + memory(arg2)).toShort)
        case MUL => memory.update(out, (memory(arg1) * memory(arg2)).toShort)
        case LOAD => memory.update(out, memory(arg1))
        case CNT => memory.update(out, arg1)
      }
    }
    memory
  }

}
