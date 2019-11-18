package pipesc

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.StandardCharsets

import Instruction._

object VM {
  def run(program: Program, args: (String, Int)*): Array[Int] = {
    val memory = Array.ofDim[Int](program.stackSize)
    val knobs = program.knobs.map {
      case (index, knob) =>
        knob.description.text -> index
    }
    for ((knob, value) <- args) {
      memory.update(knobs(knob), value)
    }
    for (i <- program.instructions) {
      i match {
        case BinaryInstruction(SET, arg1, arg2, out) =>
          if (memory(arg1) != 0) {
            memory.update(out, memory(arg2))
          }
        case BinaryInstruction(SETN, arg1, arg2, out) =>
          if (memory(arg1) == 0) {
            memory.update(out, memory(arg2))
          }
        case BinaryInstruction(ADD, arg1, arg2, out) => memory.update(out, memory(arg1) + memory(arg2))
        case BinaryInstruction(MUL, arg1, arg2, out) => memory.update(out, memory(arg1) * memory(arg2))
        case BinaryInstruction(SUB, arg1, arg2, out) => memory.update(out, memory(arg1) - memory(arg2))
        case BinaryInstruction(DIV, arg1, arg2, out) => memory.update(out, memory(arg1) / memory(arg2))
        case BinaryInstruction(MOD, arg1, arg2, out) => memory.update(out, memory(arg1) % memory(arg2))
        case UnaryInstruction(LOAD, address, out)    => memory.update(out, memory(address))
        case NullaryInstruction(CNT, constant, out)  => memory.update(out, constant.value.value)
      }
    }

    println("Result of AST run:")
    for((index, cc) <- program.controllers) {
      println(s"CC$cc = ${memory(index)}")
    }
    memory
  }

  def run(binary: ByteBuffer, args: (String, Int)*): Map[Short, Int] = {
    val argMap = Map(args: _*)

    binary.getShort() // Discard version header
    val instructions = binary.getInt() * 8 // Read number of instructions in program

    // Read actual program into array
    val program = Array.ofDim[Byte](instructions)
    binary.get(program)

    // Get stack size exponent
    val stackSize = binary.get()

    // Allocate stack
    val memory = Array.ofDim[Int](math.pow(2, stackSize).toInt)

    // Get number of groups
    val numberOfGroups = binary.get()

    // Read and discard all groups
    for(i <- 0 until numberOfGroups) {
      val rowFrom = binary.get()
      val columnFrom = binary.get()
      val rowTo = binary.get()
      val columnTo = binary.get()
      val descriptionLength = binary.get()
      val descriptionBytes = Array.ofDim[Byte](descriptionLength)
      binary.get(descriptionBytes)
      val description = new String(descriptionBytes, StandardCharsets.UTF_8)
    }

    // Get number of knobs
    val numberOfKnobs = binary.get()

    // Read all knobs and set arguments based on which knob they controll
    for(i <- 0 until numberOfKnobs) {
      val row = binary.get()
      val column = binary.get()
      val min = binary.getInt()
      val max = binary.getInt()
      val step = binary.getInt()

      val descriptionLength = binary.get()
      val descriptionBytes = Array.ofDim[Byte](descriptionLength)
      binary.get(descriptionBytes)
      val description = new String(descriptionBytes, StandardCharsets.UTF_8)
      memory.update(i, argMap(description))
    }

    val numberOfControllers = binary.getShort()

    // Read all controllers
    val controllers = Map((for(i <- 0 until numberOfControllers) yield {
      val memoryOffset = binary.getShort()
      val controller = binary.getShort()
      memoryOffset -> controller
    }): _*)

    val buffer = ByteBuffer.wrap(program).order(ByteOrder.BIG_ENDIAN)
    for (i <- 0 until (program.size / 8)) {
      val opcode = buffer.getShort()
      val arg1 = buffer.getShort()
      val arg2 = buffer.getShort()
      buffer.position(buffer.position - 4)
      val arg = buffer.getInt()
      val out = buffer.getShort()
      opcode match {
        case SET =>
          if (memory(arg1) != 0) {
            memory.update(out, memory(arg2))
          }
        case SETN =>
          if (memory(arg1) == 0) {
            memory.update(out, memory(arg2))
          }
        case ADD  => memory.update(out, (memory(arg1) + memory(arg2)))
        case MUL  => memory.update(out, (memory(arg1) * memory(arg2)))
        case LOAD => memory.update(out, memory(arg1))
        case CNT  => memory.update(out, arg1)
      }
    }
    memory

    for((memoryOffset, controller) <- controllers) yield {
      controller -> memory(memoryOffset)
    }
  }

}
