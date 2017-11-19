package scvmc

import scala.util.parsing.combinator._

object Main {
  def main(args: Array[String]) {
    val plumber = new Plumber()
    val code = """
def g(a, b, c, d):
  q = mul(add(a, b), add(d, c))

def q(b):
  a = add(b, g(1, 2, 3, 4))

def main():
  v = add(q(0),q(1))
"""
    println(code)
    PipeLexer.parse(PipeLexer.tokens, code) match {
      case PipeLexer.NoSuccess(msg, next) => println(msg)
      case PipeLexer.Success(tokens, next) =>
        println(s"Tokens: $tokens, $next")
        PipeParser.file(new PipeTokenReader(tokens)) match {
          case PipeParser.NoSuccess(msg, next) => println(msg); println(next)
          case PipeParser.Success(ast, next) =>
            println(s"AST: $ast, $next")
            val functions = ast.map(fn => fn.identifier -> fn).toMap
            val unrolled = plumber.unroll(Identifier("main"), functions)
            PipeStatement.prettyPrint(unrolled, 0)
        }
    }

  }
}
