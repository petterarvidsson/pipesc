package pipesc

import Instruction._

object VM {
  def run(program: Program, args: (String, Int)*): Array[Int] = {
    val memory = Array.ofDim[Int](program.stackSize)
    for(arg <- args) {
      memory.update(program.values(Identifier(arg._1)), arg._2)
    }
    for (i <- program.instructions) {
      i match {
        case BinaryInstruction(Add, arg1, arg2, out) => memory.update(out, memory(arg1) + memory(arg2))
        case BinaryInstruction(Mul, arg1, arg2, out) => memory.update(out, memory(arg1) * memory(arg2))
        case UnaryInstruction(Load, address, out) => memory.update(out, memory(address))
        case NullaryInstruction(Let, constant, out) => memory.update(out, constant.value.value)
      }
    }
    memory
  }
}
