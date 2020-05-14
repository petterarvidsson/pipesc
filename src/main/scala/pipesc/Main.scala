package pipesc

import scala.io.Source
import java.io.FileOutputStream
import java.io.File
import scala.util.parsing.combinator._

object Main {
  def main(args: Array[String]) {
    if (args.size != 1) {
      println("Usage: pipesc <source.pipe>")
      System.exit(1)
    }
    val code = Source.fromFile(args(0)).mkString
    val path = new File(args(0)).getCanonicalFile.toPath

    val plumber = new Plumber(path)
    PipeLexer.parse(PipeLexer.tokens, code) match {
      case PipeLexer.NoSuccess(msg, next) =>
        println(s"$path:${next.pos.line}:${next.pos.column}: $msg\n${next.pos.longString}")
      case PipeLexer.Success(tokens, next) =>
        val moduleNs = Seq("demo")
        PipeParser.file(moduleNs)(new PipeTokenReader(tokens)) match {
          case PipeParser.NoSuccess(msg, next) =>
            println(s"$path:${next.pos.line}:${next.pos.column}: $msg\n${next.pos.longString}")
          case PipeParser.Success(ast, next) =>
            val (unrolledProgram, errors) = plumber.unroll(ast)
            if (!errors.isEmpty) {
              for (error <- errors) {
                println(error.prettyPrint)
              }
              System.exit(-1)
            }
            val program = Assembler.assemble(unrolledProgram)
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
