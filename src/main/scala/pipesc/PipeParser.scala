package pipesc

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Positional, Reader}

case class NSIdentifier(namespace: Seq[Identifier], identifier: Identifier) {
  def name = namespace.map(_.name).mkString(".") + "." + identifier.name
}

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
}

sealed trait NativePipeStatement

object NativePipeStatement {
  def printIndented(s: Any, indentation: Int) {
    val indent = " " * indentation
    println(s"$indent$s")
  }
  def prettyPrint(statement: NativePipeStatement, indentation: Int) {
    statement match {
      case v: Value =>
        printIndented(v, indentation)
      case NativeFunctionApplication(identifier, arg1, arg2) =>
        printIndented(s"${identifier.name}(", indentation)
        prettyPrint(arg1, indentation + 2)
        prettyPrint(arg2, indentation + 2)
        printIndented(s")", indentation)
      case c: Constant =>
        printIndented(c, indentation)
    }
  }

}
case class FunctionApplication(identifier: NSIdentifier, arguments: Seq[PipeStatement]) extends PipeStatement
case class Value(identifier: Identifier) extends PipeStatement with NativePipeStatement
case class Constant(value: IntNum) extends PipeStatement with NativePipeStatement
case class NativeFunctionApplication(identifier: NSIdentifier, arg1: NativePipeStatement, arg2: NativePipeStatement)
    extends NativePipeStatement
case class FunctionDefinition(identifier: NSIdentifier, arguments: Seq[Identifier], statement: PipeStatement) {
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

  def arglist(namespaceSeq: Seq[Identifier]): Parser[Seq[PipeStatement]] = rep1sep(statement(namespaceSeq), Comma)

  def namespacePart = identifier ~ Dot ^^ {
    case i ~ _ => i
  }

  def namespace = namespacePart ~ namespacePart .* ~ identifier ^^ {
    case root ~ identifiers ~ identifier => NSIdentifier(root +: identifiers.toSeq, identifier)
  }

  def namespaceIdentifier(namespaceSeq: Seq[Identifier]): Parser[NSIdentifier] = (namespace | identifier) ^^ {
    case n: NSIdentifier => n
    case i: Identifier => if(Predef.is(i)) {
      NSIdentifier(Predef.NS, i)
    } else {
      NSIdentifier(namespaceSeq, i)
    }
  }

  def value = accept("identifier", { case i: Identifier => Value(i) })

  def fndefarglist: Parser[Seq[Identifier]] = repsep(identifier, Comma)

  def fnDef(namespaceSeq: Seq[Identifier]) = Def ~ namespaceIdentifier(namespaceSeq) ~ Open ~ fndefarglist ~ Close ~ Equals ~ NewLine.* ~ statement(namespaceSeq) ^^ {
    case _ ~ i ~ _ ~ args ~ _ ~ _ ~ _ ~ statement => FunctionDefinition(i, args, statement)
  }

  def fn(namespaceSeq: Seq[Identifier]) = namespaceIdentifier(namespaceSeq) ~ Open ~ arglist(namespaceSeq) ~ Close ^^ {
    case i ~ _ ~ args ~ _ => FunctionApplication(i, args)
  }

  def statement(namespaceSeq: Seq[Identifier]): Parser[PipeStatement] = fn(namespaceSeq) | constant | value

  def file(namespaceSeq: Seq[Identifier]) = phrase(rep1(fnDef(namespaceSeq) | newline)) ^^ { r =>
    r.collect {
      case fn: FunctionDefinition => fn
    }
  }
}
