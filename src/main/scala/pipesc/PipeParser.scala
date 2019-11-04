package pipesc

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Positional, Reader}

/* This avoids leaking position information into AST after parsing */
case class Positioned[T](value: T) extends Positional

case class NSIdentifier(namespace: Seq[String], identifier: String) extends Positional {
  def name = namespace.mkString(".") + "." + identifier
}

case class PipeProgram(controllers: Seq[MidiControllerDefinition],
                       knobs: Map[String, KnobDefinition],
                       groups: Map[String, GroupDefinition],
                       functions: Map[NSIdentifier, FunctionDefinition]) {
  locally {
    for {
      knob <- knobs.values
    } {
      require(groups.contains(knob.groupIdentifier), s"No such group ${knob.groupIdentifier} defined for knob ${knob.identifier}")
    }

    for {
      controller <- controllers
      argument <- controller.arguments
    } {
      require(
        knobs.contains(argument),
        s"No such knob ${argument} defined for midi controller ${controller.controller}"
      )
    }
  }
}

sealed trait PipeStatement extends Positional

object PipeStatement {
  def values(statement: PipeStatement): Set[Value] = statement match {
    case v: Value =>
      Set(v)
    case FunctionApplication(_, arguments) =>
      arguments.flatMap(values).toSet
    case _: Constant =>
      Set.empty[Value]
    case IfStatement(c, t, e) =>
      values(c) ++ values(t) ++ values(e)
  }
}

sealed trait NativePipeStatement extends Positional

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
      case NativeIfStatement(cond, arg1, arg2) =>
        printIndented("if(", indentation)
        prettyPrint(cond, indentation + 2)
        prettyPrint(arg1, indentation + 2)
        prettyPrint(arg2, indentation + 2)
        printIndented(s")", indentation)
      case c: Constant =>
        printIndented(c, indentation)
    }
  }

}
case class FunctionApplication(identifier: NSIdentifier, arguments: Seq[PipeStatement]) extends PipeStatement
case class IfStatement(cond: PipeStatement, `then`: PipeStatement, `else`: PipeStatement) extends PipeStatement
case class Value(identifier: String) extends PipeStatement with NativePipeStatement
case class Constant(value: IntNum) extends PipeStatement with NativePipeStatement
case class NativeFunctionApplication(identifier: NSIdentifier, arg1: NativePipeStatement, arg2: NativePipeStatement)
    extends NativePipeStatement
case class NativeIfStatement(cond: NativePipeStatement, `then`: NativePipeStatement, `else`: NativePipeStatement)
    extends NativePipeStatement
case class FunctionDefinition(identifier: NSIdentifier, arguments: Seq[String], statement: PipeStatement) {
  for {
    v <- PipeStatement.values(statement)
  } {
    require(
      arguments.contains(v.identifier),
      s"${v.identifier} is undefined in function ${identifier}"
    )
  }
}
case class KnobDefinition(identifier: String, groupIdentifier: String, row: Int, column: Int, description: Text, from: Int, to: Int)
case class MidiControllerDefinition(controller: Int, arguments: Seq[String], statement: PipeStatement) {
  for {
    v <- PipeStatement.values(statement)
  } {
    require(
      arguments.contains(v.identifier),
      s"${v.identifier} is undefined in definition of midi controller ${controller}"
    )
  }
}
case class GroupDefinition(identifier: String, description: Text, rowFrom: Int, columnFrom: Int, rowTo: Int, columnTo: Int)
object PipeParser extends Parsers {
  override type Elem = PipeToken

  def identifier = positioned(accept("identifier", { case id: Identifier => id }))

  def constant = positioned(accept("intnum", { case i: IntNum => Constant(i) }))

  def text = positioned(accept("text", { case t: Text => t }))

  def newline = positioned(accept("newline", { case n: NewLine => n }))

  def `def` = positioned(accept("def", { case d: Def => d }))

  def `if` = positioned(accept("if", { case i: If => i }))

  def open = positioned(accept("open", { case o: Open => o }))

  def close = positioned(accept("close", { case c: Close => c }))

  def comma = positioned(accept("comma", { case c: Comma => c }))

  def equals = positioned(accept("equals", { case e: Equals => e }))

  def knob = positioned(accept("knob", { case k: Knob => k }))

  def group = positioned(accept("group", { case g: Group => g }))

  def midicontroller = positioned(accept("midicontroller", { case m: MidiController => m }))

  def arglist(namespaceSeq: Seq[String]): Parser[Positioned[Seq[PipeStatement]]] =
    positioned(rep1sep(statement(namespaceSeq), comma) ^^ (Positioned(_)))

  def namespacePart =
    positioned(identifier ~ Dot() ^^ {
      case i ~ _ => i
    })

  def namespace =
    positioned(namespacePart ~ namespacePart.* ~ identifier ^^ {
      case root ~ identifiers ~ identifier => NSIdentifier(root.name +: identifiers.map(_.name).toSeq, identifier.name)
    })

  def namespaceIdentifier(namespaceSeq: Seq[String]): Parser[NSIdentifier] =
    positioned((namespace | identifier) ^^ {
      case n: NSIdentifier => n
      case i: Identifier =>
        if (Predef.is(i.name)) {
          NSIdentifier(Predef.NS, i.name)
        } else {
          NSIdentifier(namespaceSeq, i.name)
        }
    })

  def value = positioned(accept("identifier", { case i: Identifier => Value(i.name) }))

  def fndefarglist: Parser[Positioned[Seq[Identifier]]] = positioned(repsep(identifier, comma) ^^ (Positioned(_)))

  def fnDef(namespaceSeq: Seq[String]) =
    positioned(
      `def` ~ namespaceIdentifier(namespaceSeq) ~ open ~ fndefarglist ~ close ~ equals ~ newline.* ~ statement(
        namespaceSeq) ^^ {
        case _ ~ i ~ _ ~ args ~ _ ~ _ ~ _ ~ statement =>
          Positioned(FunctionDefinition(i, args.value.map(_.name), statement))
      })

  def knobDef =
    positioned(knob ~ identifier ~ open ~ identifier ~ comma ~ constant ~ comma ~ constant ~ comma ~ text ~ comma ~ constant ~ comma ~ constant ~ close ^^ {
      case _ ~ i ~ _ ~ g ~ _ ~ row ~ _ ~ column ~ _ ~ description ~ _ ~ from ~ _ ~ to ~ _ =>
        Positioned(KnobDefinition(i.name, g.name, row.value.value, column.value.value, description, from.value.value, to.value.value))
    })

  def groupDef =
    positioned(group ~ identifier ~ open ~ text ~ comma ~ constant ~ comma ~ constant ~ comma ~ constant ~ comma ~ constant~ close ^^ {
      case _ ~ i ~ _ ~ description ~ _ ~ fromRow ~ _ ~ fromColumn ~ _ ~ toRow ~ _ ~ toColumn ~ _ =>
          Positioned(GroupDefinition(i.name, description, fromRow.value.value, fromColumn.value.value, toRow.value.value, toColumn.value.value))
    })

  def midiControllerDef(namespaceSeq: Seq[String]) =
    positioned(
      midicontroller ~ open ~ constant ~ close ~ open ~ fndefarglist ~ close ~ equals ~ newline.* ~ statement(
        namespaceSeq) ^^ {
        case _ ~ _ ~ controller ~ _ ~ _ ~ args ~ _ ~ _ ~ _ ~ statement =>
          Positioned(MidiControllerDefinition(controller.value.value, args.value.map(_.name), statement))
      })

  def fn(namespaceSeq: Seq[String]) =
    positioned(namespaceIdentifier(namespaceSeq) ~ open ~ arglist(namespaceSeq) ~ close ^^ {
      case i ~ _ ~ args ~ _ => FunctionApplication(i, args.value)
    })

  def ifa(namespaceSeq: Seq[String]) =
    positioned(
      `if` ~ open ~ statement(namespaceSeq) ~ comma ~ statement(namespaceSeq) ~ comma ~ statement(namespaceSeq) ~ close ^^ {
        case _ ~ _ ~ c ~ _ ~ t ~ _ ~ e ~ _ => IfStatement(c, t, e)
      })

  def statement(namespaceSeq: Seq[String]): Parser[PipeStatement] =
    positioned(ifa(namespaceSeq) | fn(namespaceSeq) | constant | value)

  def file(namespaceSeq: Seq[String]): Parser[Positioned[PipeProgram]] =
    positioned(phrase(rep1(knobDef | midiControllerDef(namespaceSeq) | fnDef(namespaceSeq) | groupDef | newline)) ^^ { r =>
      Positioned(
        PipeProgram(
          r.collect {
            case Positioned(controller: MidiControllerDefinition) => controller
          },
          Map(r.collect {
            case Positioned(knob: KnobDefinition) => knob.identifier -> knob
          }: _*),
          Map(r.collect {
            case Positioned(group: GroupDefinition) => group.identifier -> group
          }: _*),
          Map(r.collect {
            case Positioned(fn: FunctionDefinition) => fn.identifier -> fn
          }: _*)
        ))
    })
}
