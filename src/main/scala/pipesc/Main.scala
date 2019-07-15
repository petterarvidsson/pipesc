package pipesc

import scala.io.Source
import java.io.FileOutputStream
import scala.util.parsing.combinator._

object Main {
  def main(args: Array[String]) {
    val plumber = new Plumber()
    if(args.size != 1) {
      println("Usage: pipesc <source.pipe>")
      System.exit(1)
    }
    val code = Source.fromFile(args(0)).mkString
    println(code)
    PipeLexer.parse(PipeLexer.tokens, code) match {
      case PipeLexer.NoSuccess(msg, next) => println(msg)
      case PipeLexer.Success(tokens, next) =>
        println(s"Tokens: $tokens, $next")
        val moduleNs = Seq(Identifier("demo"))
        PipeParser.file(moduleNs)(new PipeTokenReader(tokens)) match {
          case PipeParser.NoSuccess(msg, next) => println(msg); println(next)
          case PipeParser.Success(ast, next) =>
            println(s"AST: $ast, $next")
            val functions = ast.map(fn => fn.identifier -> fn).toMap
            println(functions)
            val entry = plumber.unroll(NSIdentifier(moduleNs, Identifier("main")), functions)
            EntryPoint.prettyPrint(entry)
            val program = Assembler.assemble(entry)
            Program.prettyPrint(program)
            println(VM.run(program, "a" -> 1, "b" -> 2).toSeq)
            val binary = Binary.binaryEncode(program)
            binary.rewind()
            val instructions = binary.getInt() * 8
            val bytes = Array.ofDim[Byte](instructions)
            binary.get(bytes)
            val stackSize = binary.getShort()
            println(VM.run(bytes, stackSize, Array[Short](1, 2)).toSeq)
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
