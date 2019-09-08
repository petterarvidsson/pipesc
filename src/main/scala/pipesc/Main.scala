package pipesc

import scala.io.Source
import java.io.FileOutputStream
import scala.util.parsing.combinator._

object Main {
  def main(args: Array[String]) {
    val plumber = new Plumber()
    if (args.size != 1) {
      println("Usage: pipesc <source.pipe>")
      System.exit(1)
    }
    val code = Source.fromFile(args(0)).mkString
    println(code)
    PipeLexer.parse(PipeLexer.tokens, code) match {
      case PipeLexer.NoSuccess(msg, next) => println(s"${next.pos.line}:${next.pos.column} $msg")
      case PipeLexer.Success(tokens, next) =>
        println(s"Tokens: $tokens, $next")
        val moduleNs = Seq("demo")
        PipeParser.file(moduleNs)(new PipeTokenReader(tokens)) match {
          case PipeParser.NoSuccess(msg, next) => println(s"${next.pos.line}:${next.pos.column} $msg")
          case PipeParser.Success(Positioned(ast), next) =>
            println(s"AST: $ast, $next")
            val unrolledProgram = plumber.unroll(ast)
            UnrolledPipeProgram.prettyPrint(unrolledProgram)
            val program = Assembler.assemble(unrolledProgram)
            Program.prettyPrint(program)
            println(VM.run(program, "cut_off" -> 2, "resonance" -> 4).toSeq)
            val binary = Binary.binaryEncode(program)
            binary.rewind()
            val instructions = binary.getInt() * 8
            val bytes = Array.ofDim[Byte](instructions)
            binary.get(bytes)
            val stackSize = binary.getShort()
            println(VM.run(bytes, stackSize, Array[Short](2, 4)).toSeq)
            binary.rewind()
            val stream = new FileOutputStream("program.bin")
            val fileBytes = Array.ofDim[Byte](4 + instructions + 2)
            binary.get(fileBytes)
            stream.write(fileBytes);
            stream.close()
        }
    }

  }
}
