package pipesc

import scala.util.parsing.input.OffsetPosition

import org.scalatest._

class languageSpec extends FlatSpec with Matchers {

  def compileToProgram(code: String): Either[Seq[CompilerError], UnrolledPipeProgram] =
    for {
      tokens <- PipeLexer.parseString(code)
      ast <- PipeParser.parseTokens(tokens, Seq("test"))
      program <- Plumber.parseAst(ast)
    } yield {
      program
    }

  "The compiler" should "unroll function applications" in {
    val code = """
  def f(int a) =
    a

  midi_cc(74)() =
    f(1)
  """

    compileToProgram(code) shouldEqual Right(
      UnrolledPipeProgram(List(UnrolledCC(74, Constant(1), Set())), Map.empty, Map.empty))

  }

  it should "unroll function applications inside if" in {
    val code = """
  def f(int a) =
    a

  midi_cc(74)() =
    if(1, f(1), f(2))
  """

    compileToProgram(code) shouldEqual Right(
      UnrolledPipeProgram(List(UnrolledCC(74, NativeIfStatement(Constant(1), Constant(1), Constant(2)), Set())),
                          Map.empty,
                          Map.empty))

  }

  it should "not allow direct recursion" in {
    val code = """
  def f(int a) =
    f(a)

  midi_cc(74)() =
    f(1)
  """

    compileToProgram(code) shouldEqual Left(
      Seq(CompilerError(OffsetPosition(code, 22), "Recursive call of test.f detected")))

  }

  it should "not allow indirect recursion" in {
    val code = """
  def g(int a) =
    f(a)

  def f(int a) =
    g(a)

  midi_cc(74)() =
    f(1)
  """

    compileToProgram(code) shouldEqual Left(
      Seq(CompilerError(OffsetPosition(code, 22), "Recursive call of test.f detected")))

  }

}
