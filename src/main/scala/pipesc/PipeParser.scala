package pipesc

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Positional, Reader}

sealed trait PipeStatement

object PipeStatement {
  def values(statement: PipeStatement): Set[Value] = statement match {
    case v: Value =>
      Set(v)
    case FunctionApplication(_, arguments) =>
      arguments.flatMap(values).toSet
    case _: Constant =>
      Set.empty[Value]
  }
  def printIndented(s: Any, indentation: Int) {
    val indent = " " * indentation
    println(s"$indent$s")
  }
  def prettyPrint(statement: PipeStatement, indentation: Int) {
    statement match {
      case v: Value =>
        printIndented(v, indentation)
      case FunctionApplication(identifier, arguments) =>
        printIndented(s"${identifier.name}(", indentation)
        for (argument <- arguments) {
          prettyPrint(argument, indentation + 2)
        }
        printIndented(s")", indentation)
      case c: Constant =>
        printIndented(c, indentation)
    }
  }
}
case class FunctionApplication(identifier: Identifier, arguments: Seq[PipeStatement]) extends PipeStatement
case class Value(identifier: Identifier) extends PipeStatement
case class Constant(value: IntNum) extends PipeStatement

case class FunctionDefinition(identifier: Identifier, arguments: Seq[Identifier], statement: PipeStatement) {
  for {
    v <- PipeStatement.values(statement)
  } {
    require(
      arguments.contains(v.identifier),
      s"${v.identifier.pos}: ${v.identifier.name} is undefined in function ${identifier.name}"
    )
  }
}

object PipeParser extends Parsers {
  override type Elem = PipeToken

  def identifier = accept("identifier", { case id: Identifier => id })

  def constant = accept("intnum", { case i: IntNum => Constant(i) })

  def newline = accept("newline", { case NewLine => NewLine })

  def arglist: Parser[Seq[PipeStatement]] = rep1sep(statement, Comma)

  def value = accept("identifier", { case i: Identifier => Value(i) })

  def fndefarglist: Parser[Seq[Identifier]] = repsep(identifier, Comma)

  def fnDef = Def ~ identifier ~ Open ~ fndefarglist ~ Close ~ Equals ~ NewLine.* ~ statement ^^ {
    case _ ~ i ~ _ ~ args ~ _ ~ _ ~ _ ~ statement => FunctionDefinition(i, args, statement)
  }

  def fn = identifier ~ Open ~ arglist ~ Close ^^ {
    case i ~ _ ~ args ~ _ => FunctionApplication(i, args)
  }

  def statement: Parser[PipeStatement] = fn | constant | value

  def file = phrase(rep1(fnDef | newline)) ^^ { r =>
    r.collect {
      case fn: FunctionDefinition => fn
    }
  }
}
