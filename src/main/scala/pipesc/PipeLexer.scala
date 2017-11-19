package pipesc

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Positional, Reader}

sealed trait PipeToken

case class Identifier(name: String) extends PipeToken with Positional
case class Native(name: String) extends PipeToken with Positional
case class IntNum(value: Int) extends PipeToken with Positional
case object Open extends PipeToken
case object Close extends PipeToken
case object NewLine extends PipeToken
case object Comma extends PipeToken
case object Dot extends PipeToken
case object Colon extends PipeToken
case object Equals extends PipeToken
case object Def extends PipeToken

object PipeLexer extends RegexParsers {
  // Tokens
  def kdef = "def" ^^ { _ =>
    Def
  }
  def identifier = positioned("""[a-zA-Z_][a-zA-Z0-9_]*""".r ^^ { s =>
    Identifier(s.toString)
  })
  def intnum = positioned("""[0-9]+""".r ^^ { s =>
    IntNum(s.toInt)
  })
  def open = "(" ^^ { _ =>
    Open
  }
  def close = ")" ^^ { _ =>
    Close
  }
  def comma = "," ^^ { _ =>
    Comma
  }
  def colon = ":" ^^ { _ =>
    Colon
  }
  def equals = "=" ^^ { _ =>
    Equals
  }
  def newline = """[\r\f\n]""".r ^^ { _ =>
    NewLine
  }
  def tokens: Parser[List[PipeToken]] = {
    phrase(rep1( intnum | kdef | identifier | open | close | comma | equals | colon | newline )) ^^ {r => r}
  }

  override def skipWhitespace = true
  override val whiteSpace = "[ \t]+".r

}

class PipeTokenReader(tokens: Seq[PipeToken]) extends Reader[PipeToken] {
  override def first: PipeToken = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[PipeToken] = new PipeTokenReader(tokens.tail)
}
