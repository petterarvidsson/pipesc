package pipesc

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Positional, Reader}

/* This avoids leaking position information into AST after parsing */
case class Positioned[T](value: T) extends Positional

case class NSIdentifier(namespace: Seq[String], identifier: String) extends Positional {
  def name =
    if (namespace == Seq("predef")) {
      identifier
    } else {
      namespace.mkString(".") + "." + identifier
    }
}

case class PipeProgram(controllers: Seq[MidiControllerDefinition],
                       knobs: Map[String, KnobDefinition],
                       groups: Map[String, GroupDefinition],
                       functions: Map[NSIdentifier, FunctionDefinition])

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

case class MinMax(min: Int, max: Int) {
  def withinBounds(m: MinMax): Boolean =
    m.max <= max && m.min >= min
}

sealed trait NativePipeStatement extends Positional {
  def minMax: MinMax
}

object NativePipeStatement {
  def printIndented(s: Any, indentation: Int) {
    val indent = " " * indentation
    println(s"$indent$s")
  }

  def minMax(statement: NativePipeStatement): String =
    s"[${statement.minMax.min}, ${statement.minMax.max}]"

  def pos(positional: Positional): String =
    s"${positional.pos}"

  def prettyPrint(statement: NativePipeStatement, indentation: Int) {
    statement match {
      case s @ KnobDefinition(identifier, _, _, _, _, _, _, _) =>
        printIndented(s"$identifier ${minMax(s)} ${pos(s)}", indentation)
      case s @ NativeFunctionApplication(identifier, arg1, arg2) =>
        printIndented(s"${identifier.name}(", indentation)
        prettyPrint(arg1, indentation + 2)
        prettyPrint(arg2, indentation + 2)
        printIndented(s") ${minMax(s)} ${pos(identifier)}", indentation)
      case s @ NativeIfStatement(cond, arg1, arg2) =>
        printIndented("if(", indentation)
        prettyPrint(cond, indentation + 2)
        prettyPrint(arg1, indentation + 2)
        prettyPrint(arg2, indentation + 2)
        printIndented(s") ${minMax(s)} ${pos(s)}", indentation)
      case c: Constant =>
        printIndented(s"${c.value.value} ${minMax(c)} ${pos(c)}", indentation)
    }
  }

}

case class FunctionApplication(identifier: NSIdentifier, arguments: Seq[PipeStatement]) extends PipeStatement

case class IfStatement(cond: PipeStatement, `then`: PipeStatement, `else`: PipeStatement) extends PipeStatement

case class Value(identifier: String) extends PipeStatement

case class Constant(value: IntNum) extends PipeStatement with NativePipeStatement {
  def minMax = MinMax(value.value, value.value)
}

case class NativeFunctionApplication(identifier: NSIdentifier, arg1: NativePipeStatement, arg2: NativePipeStatement)
    extends NativePipeStatement {
  def minMax = Instruction.i2minMax(identifier)(arg1.minMax, arg2.minMax)
}

case class NativeIfStatement(cond: NativePipeStatement, `then`: NativePipeStatement, `else`: NativePipeStatement)
    extends NativePipeStatement {
  def minMax = {
    MinMax(math.min(`then`.minMax.min, `else`.minMax.min), math.max(`then`.minMax.max, `else`.minMax.max))
  }
}

case class FunctionDefinition(identifier: NSIdentifier, arguments: Seq[String], statement: PipeStatement)
    extends Positional

case class KnobDefinition(identifier: String,
                          groupIdentifier: String,
                          row: Int,
                          column: Int,
                          description: Text,
                          min: Int,
                          max: Int,
                          step: Int)
    extends NativePipeStatement {
  def minMax = MinMax(min, max)
}
case class MidiControllerDefinition(controller: Int, arguments: Seq[String], statement: PipeStatement)
    extends Positional

case class GroupDefinition(identifier: String,
                           description: Text,
                           rowFrom: Int,
                           columnFrom: Int,
                           rowTo: Int,
                           columnTo: Int)

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
    positioned {
      `def` ~ namespaceIdentifier(namespaceSeq) ~ open ~ fndefarglist ~ close ~ equals ~ newline.* ~ statement(
        namespaceSeq) flatMap {
        case _ ~ i ~ _ ~ args ~ _ ~ _ ~ _ ~ statement =>
          val arguments = args.value.map(_.name)

          val unidentified = for {
            v <- PipeStatement.values(statement) if !arguments.contains(v.identifier)
          } yield {
            s"${v.identifier} is undefined in function ${i.name}"
          }

          if (unidentified.isEmpty) {
            success(FunctionDefinition(i, arguments, statement))
          } else {
            err(unidentified.mkString("\n"))
          }
      }
    }

  def knobDef =
    positioned(
      knob ~ identifier ~ open ~ identifier ~ comma ~ constant ~ comma ~ constant ~ comma ~ text ~ comma ~ constant ~ comma ~ constant ~ comma ~ constant ~ close ^^ {
        case _ ~ i ~ _ ~ g ~ _ ~ row ~ _ ~ column ~ _ ~ description ~ _ ~ min ~ _ ~ max ~ _ ~ step ~ _ =>
          KnobDefinition(i.name,
                         g.name,
                         row.value.value,
                         column.value.value,
                         description,
                         min.value.value,
                         max.value.value,
                         step.value.value)
      })

  def groupDef =
    positioned(
      group ~ identifier ~ open ~ text ~ comma ~ constant ~ comma ~ constant ~ comma ~ constant ~ comma ~ constant ~ close ^^ {
        case _ ~ i ~ _ ~ description ~ _ ~ fromRow ~ _ ~ fromColumn ~ _ ~ toRow ~ _ ~ toColumn ~ _ =>
          Positioned(
            GroupDefinition(i.name,
                            description,
                            fromRow.value.value,
                            fromColumn.value.value,
                            toRow.value.value,
                            toColumn.value.value))
      })

  def midiControllerDef(namespaceSeq: Seq[String]) =
    positioned {
      midicontroller ~ open ~ constant ~ close ~ open ~ fndefarglist ~ close ~ equals ~ newline.* ~ statement(
        namespaceSeq) flatMap {
        case _ ~ _ ~ controller ~ _ ~ _ ~ args ~ _ ~ _ ~ _ ~ statement =>
          val arguments = args.value.map(_.name)
          val unidentified = for {
            v <- PipeStatement.values(statement) if !arguments.contains(v.identifier)
          } yield {
            s"${v.identifier} is undefined in definition of midi controller ${controller.value.value}"
          }
          if (unidentified.isEmpty) {
            success(MidiControllerDefinition(controller.value.value, arguments, statement))
          } else {
            err(unidentified.mkString("\n"))
          }
      }
    }

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

  def file(namespaceSeq: Seq[String]): Parser[PipeProgram] =
    phrase(rep1(knobDef | midiControllerDef(namespaceSeq) | fnDef(namespaceSeq) | groupDef | newline)) flatMap { defs =>
      val controllers = defs.collect {
        case controller: MidiControllerDefinition => controller
      }

      val knobs = Map(defs.collect {
        case knob: KnobDefinition => knob.identifier -> knob
      }: _*)

      val groups = Map(defs.collect {
        case Positioned(group: GroupDefinition) => group.identifier -> group
      }: _*)

      val functions = Map(defs.collect {
        case fn: FunctionDefinition => fn.identifier -> fn
      }: _*)

      val undefinedGroups = for {
        knob <- knobs.values if !groups.contains(knob.groupIdentifier)
      } yield {
        s"No such group ${knob.groupIdentifier} defined for knob ${knob.identifier}"
      }

      val undefinedKnobs = for {
        controller <- controllers
        argument <- controller.arguments if !knobs.contains(argument)
      } yield {
        s"No such knob ${argument} defined for midi controller ${controller.controller}"
      }

      val undefined = undefinedGroups ++ undefinedKnobs

      if (undefined.isEmpty) {
        success(PipeProgram(controllers, knobs, groups, functions))
      } else {
        err(undefined.mkString("\n"))
      }
    }
}

class PipeTokenReader(tokens: Seq[PipeToken]) extends Reader[PipeToken] {
  override def first: PipeToken = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)
  override def rest: Reader[PipeToken] = new PipeTokenReader(tokens.tail)
}
