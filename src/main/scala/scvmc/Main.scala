package scvmc

import scala.util.parsing.combinator._

object Main {
  def main(args: Array[String]) {
    val code = """
def q(b):
a = b

def v(a, b, c, d):
r = sub(add(q(a), b), d)
y = add(r, 2)
"""
    println(code)
    PipeLexer.parse(PipeLexer.tokens, code) match {
      case PipeLexer.NoSuccess(msg, next) => println(msg)
      case PipeLexer.Success(tokens, next) =>
        println(tokens)
        println(PipeParser.file(new PipeTokenReader(tokens)))
    }

  }
}
