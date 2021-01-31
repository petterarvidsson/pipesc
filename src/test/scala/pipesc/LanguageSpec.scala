package pipesc

import scala.util.parsing.input.OffsetPosition

import org.scalatest._

class LanguageSpec extends FlatSpec with Matchers {

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
      UnrolledPipeProgram(List(UnrolledMidiDefinition(MidiCC(74), Constant(1), Set())), Map.empty, Map.empty))

  }

  it should "unroll function applications inside if" in {
    val code = """
  def f(int a) =
    a

  midi_cc(74)() =
    if(1, f(1), f(2))
  """

    compileToProgram(code) shouldEqual Right(
      UnrolledPipeProgram(List(UnrolledMidiDefinition(MidiCC(74), NativeIfStatement(Constant(1), Constant(1), Constant(2)), Set())),
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

  it should "not allow division with zero" in {
    val code = """
  midi_cc(74)() =
    div(10,0)
  """

    compileToProgram(code).isLeft shouldEqual true
  }

  it should "not allow calling a function outside its defined range" in {
    val code = """
  def f(int[10..12] a) =
    a

  midi_cc(74)() =
    f(13)
  """

    compileToProgram(code).isLeft shouldEqual true
  }

  it should "keep ranges disjoint" in {
    val code = """
  def f(int[-1..-1] a, int[1..1] b) =
    div(1, if(1, a, b))

  midi_cc(74)() =
    add(f(-1, 1), 1)
  """

    compileToProgram(code).isRight shouldEqual true
  }

  it should "generate scaling code" in {
    val code = """
  group volume("Volume", 2, 0, 3, 1)

  controller amp(volume, 0, 0, "", 0, 1023, 1)

  midi_cc(74)(amp) =
    scale(amp)
  """

    val scaleCode = """
  group volume("Volume", 2, 0, 3, 1)

  controller amp(volume, 0, 0, "", 0, 1023, 1)

  midi_cc(74)(amp) =
    div(add(amp, 0), 9)
  """

    compileToProgram(code) shouldEqual compileToProgram(scaleCode)
  }

  it should "not generate scaling code that scales up (i.e. division by zero)" in {
    val code = """
  group volume("Volume", 2, 0, 3, 1)

  controller amp(volume, 0, 0, "", 0, 3, 1)

  midi_cc(74)(amp) =
    scale(amp)
  """

    val scaleCode = """
  group volume("Volume", 2, 0, 3, 1)

  controller amp(volume, 0, 0, "", 0, 3, 1)

  midi_cc(74)(amp) =
    div(add(amp, 0), 1)
  """

    compileToProgram(code) shouldEqual compileToProgram(scaleCode)
  }


}
