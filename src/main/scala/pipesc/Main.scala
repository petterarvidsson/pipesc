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
          case PipeParser.Success(ast, next) =>
            println(s"AST: $ast, $next")
            val unrolledProgram = plumber.unroll(ast)
            UnrolledPipeProgram.prettyPrint(unrolledProgram)
            val program = Assembler.assemble(unrolledProgram)
            Program.prettyPrint(program)
            println(
              VM.run(program,
                     "Cut Off" -> 2,
                     "Resonance" -> 4,
                     "" -> 5,
                     "Attack" -> 6,
                     "Decay" -> 7,
                     "Sustain" -> 8,
                     "Release" -> 9)
                .toSeq)
            val binary = Binary.binaryEncode(program)
            val size = binary.position

            // Run binary program
            binary.rewind()
            println(
              VM.run(binary,
                     "Cut Off" -> 2,
                     "Resonance" -> 4,
                     "" -> 5,
                     "Attack" -> 6,
                     "Decay" -> 7,
                     "Sustain" -> 8,
                     "Release" -> 9)
                .toSeq)

            // Write binary to file
            binary.rewind()
            val stream = new FileOutputStream("program.bin")
            val fileBytes = Array.ofDim[Byte](size)
            binary.get(fileBytes)
            stream.write(fileBytes);
            stream.close()

        }
    }

  }
}
