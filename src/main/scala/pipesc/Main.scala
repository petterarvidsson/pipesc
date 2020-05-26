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
    val file = new File(args(0)).getCanonicalFile
    val path = file.toPath
    val module = file.getName.replaceAll("(\\.[^\\.]*$)", "")
    val moduleNs = Seq(module)

    val result: Either[Seq[CompilerError], Array[Byte]] = for {
      tokens <- PipeLexer.parseString(code)
      ast <- PipeParser.parseTokens(tokens, moduleNs)
      program <- Plumber.parseAst(ast)
    } yield {
      val native = Assembler.assemble(program)
      val binary = Binary.binaryEncode(native)
      val size = binary.position
      binary.rewind()
      val fileBytes = Array.ofDim[Byte](size)
      binary.get(fileBytes)
      fileBytes
    }

    result match {
      case Right(bytes) =>
        val stream = new FileOutputStream("a.bin")
        try {
          stream.write(bytes)
        } finally {
          stream.close()
        }
      case Left(errors) =>
        for (error <- errors) {
          println(error.prettyPrint(path))
        }
        System.exit(-1)
    }
  }
}
