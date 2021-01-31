package pipesc

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Positional, Reader}

sealed trait PipeToken extends Positional

case class Identifier(name: String) extends PipeToken
case class Text(text: String) extends PipeToken
case class Native(name: String) extends PipeToken
case class IntNum(value: Int) extends PipeToken
case class Open() extends PipeToken
case class Close() extends PipeToken
case class SquareOpen() extends PipeToken
case class SquareClose() extends PipeToken
case class NewLine() extends PipeToken
case class Comma() extends PipeToken
case class Dot() extends PipeToken
case class Minus() extends PipeToken
case class Tilde() extends PipeToken
case class Equals() extends PipeToken
case class Def() extends PipeToken
case class If() extends PipeToken
case class IntToken() extends PipeToken
case class Controller() extends PipeToken
case class Group() extends PipeToken
case class MidiCCToken() extends PipeToken
case class MidiRPNToken() extends PipeToken
case class MidiNRPNToken() extends PipeToken
case class QuotationMark() extends PipeToken

object PipeLexer extends RegexParsers {
  // Tokens
  def kdef =
    positioned("def" ^^ { _ =>
      Def()
    })
  def kif =
    positioned("if" ^^ { _ =>
      If()
    })
  def int =
    positioned("int" ^^ { _ =>
      IntToken()
    })
  def controller =
    positioned("controller" ^^ { _ =>
      Controller()
    })
  def group =
    positioned("group" ^^ { _ =>
      Group()
    })
  def midicc =
    positioned("midi_cc" ^^ { _ =>
      MidiCCToken()
    })
  def midirpn =
    positioned("midi_rpn" ^^ { _ =>
      MidiRPNToken()
    })
  def midinrpn =
    positioned("midi_nrpn" ^^ { _ =>
      MidiNRPNToken()
    })
  def text =
    positioned("""\"[a-zA-Z_0-9\s-]*\"""".r ^^ { s =>
      val str = s.toString
      Text(str.slice(1, str.length - 1))
    })
  def identifier =
    positioned("""[a-zA-Z_][a-zA-Z0-9_]*""".r ^^ { s =>
      Identifier(s.toString)
    })
  def intnum =
    positioned("""[0-9]+""".r ^^ { s =>
      IntNum(s.toInt)
    })
  def minus =
    positioned("-" ^^ { _ =>
      Minus()
    })
  def dot =
    positioned("." ^^ { _ =>
      Dot()
    })
  def tilde =
    positioned("~" ^^ { _ =>
      Tilde()
    })
  def open =
    positioned("(" ^^ { _ =>
      Open()
    })
  def close =
    positioned(")" ^^ { _ =>
      Close()
    })
  def squareopen =
    positioned("[" ^^ { _ =>
      SquareOpen()
    })
  def squareclose =
    positioned("]" ^^ { _ =>
      SquareClose()
    })
  def comma =
    positioned("," ^^ { _ =>
      Comma()
    })
  def equals =
    positioned("=" ^^ { _ =>
      Equals()
    })
  def newline =
    positioned("""[\r\f\n]""".r ^^ { _ =>
      NewLine()
    })
  def tokens: Parser[List[PipeToken]] = {
    phrase(rep1(
      intnum | kdef | kif | controller | group | midicc | midirpn | midinrpn | int | text | identifier | dot | open | close | squareopen | squareclose | comma | equals | tilde | minus | newline)) ^^ {
      r =>
        r
    }
  }
  def parseString(code: String): Either[Seq[CompilerError], List[PipeToken]] =
    parse(tokens, code) match {
      case PipeLexer.NoSuccess(msg, next) =>
        Left(Seq(CompilerError(next.pos, msg)))
      case PipeLexer.Success(tokens, next) =>
        Right(tokens)
    }

  override def skipWhitespace = true
  override val whiteSpace = "[ \t]+".r

}
