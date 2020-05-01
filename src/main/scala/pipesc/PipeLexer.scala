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
case class Equals() extends PipeToken
case class Def() extends PipeToken
case class If() extends PipeToken
case class Knob() extends PipeToken
case class Group() extends PipeToken
case class MidiController() extends PipeToken
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
  def knob =
    positioned("knob" ^^ { _ =>
      Knob()
    })
  def group =
    positioned("group" ^^ { _ =>
      Group()
    })
  def midicontroller =
    positioned("midi_controller" ^^ { _ =>
      MidiController()
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
    positioned("""-?[0-9]+""".r ^^ { s =>
      IntNum(s.toInt)
    })
  def dot =
    positioned("." ^^ { _ =>
      Dot()
    })
  def open =
    positioned("(" ^^ { _ =>
      Open()
    })
  def close =
    positioned(")" ^^ { _ =>
      Close()
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
      intnum | kdef | kif | knob | group | midicontroller | text | identifier | dot | open | close | comma | equals | newline)) ^^ {
      r =>
        r
    }
  }

  override def skipWhitespace = true
  override val whiteSpace = "[ \t]+".r

}
