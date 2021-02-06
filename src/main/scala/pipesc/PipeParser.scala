package pipesc

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Positional, Reader}

/* This avoids leaking position information into AST after parsing */
case class Positioned[T](value: T) extends Positional

case class NSIdentifier(namespace: Seq[String], identifier: String) extends Positional {
  def name =
    if (namespace == Predef.NS) {
      identifier
    } else {
      namespace.mkString(".") + "." + identifier
    }
}

case class PipeProgram(ccs: Seq[MidiDefinition],
                       controllers: Map[String, ControllerDefinition],
                       groups: Map[String, GroupDefinition],
                       functions: Map[NSIdentifier, Function])

sealed trait Type extends Positional {
  def intervals: Seq[MinMax]
  def prettyPrint: String
}

object Type {

  def merge(intervals: Seq[MinMax]): Seq[MinMax] =
    intervals.sortBy(_.min) match {
      case head :: tail =>
        val (m, e) = tail.foldLeft((Seq.empty[MinMax], head)) { (acc, e) =>
          val (mergedIntervals, last) = acc
          if (MinMax.overlaps(e, last)) {
            (mergedIntervals, MinMax.merge(last, e))
          } else {
            (mergedIntervals :+ last, e)
          }
        }
        m :+ e
      case l => l
    }


  def merge(t1: Type, t2: Type): Type =
    IntegerType(merge(t1.intervals ++ t2.intervals))

  def signatureString(signature: Seq[Type]): String = {
    val s = signature.map(_.prettyPrint).mkString(", ")
    s"($s)"
  }
  def withinBounds(t1: Type, t2: Type): Boolean =
    t1.intervals.foldLeft(true) { (acc, bound) =>
      acc & MinMax.withinBounds(bound, t2.intervals)
    }
}

case class IntegerType(override val intervals: Seq[MinMax]) extends Type {
  def prettyPrint = {
    val intervalsString = intervals.map(_.prettyPrint).mkString(", ")
    s"int[$intervalsString]"
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

  def functionApplications(statement: PipeStatement): Set[FunctionApplication] = statement match {
    case v: Value =>
      Set.empty[FunctionApplication]
    case f @ FunctionApplication(_, arguments) =>
      Set(f) ++ arguments.flatMap(functionApplications).toSet
    case _: Constant =>
      Set.empty[FunctionApplication]
    case IfStatement(c, t, e) =>
      functionApplications(c) ++ functionApplications(t) ++ functionApplications(e)
  }
}

case class MinMax(min: Int, max: Int) {
  def offset = 0 - min
  def magnitude = max - min
  def withinBounds(m: MinMax): Boolean =
    m.max <= max && m.min >= min
  def withinBounds(i: Int): Boolean =
    i <= max && i >= min
  def prettyPrint =
    if (min == max) {
      s"$min"
    } else {
      s"$min..$max"
    }
}

object MinMax {
  def overlaps(m1: MinMax, m2: MinMax): Boolean =
    (m1.min <= (m2.max + 1) && m1.min >= (m2.min - 1)) ||
      (m1.max <= (m2.max + 1) && m1.max >= (m2.min - 1)) ||
      (m2.min <= (m1.max + 1) && m2.min >= (m1.min - 1)) ||
      (m2.max <= (m1.max + 1) && m2.max >= (m1.min - 1))

  def merge(m1: MinMax, m2: MinMax): MinMax =
    MinMax(math.min(m1.min, m2.min), math.max(m1.max, m2.max))

  def withinBounds(minMax: MinMax, bounds: Seq[MinMax]): Boolean =
    bounds.foldLeft(false) { (acc, bound) =>
      acc | bound.withinBounds(minMax)
    }
}

sealed trait NativePipeStatement extends Positional {
  def returnType: Type
}

object NativePipeStatement {
  def printIndented(s: Any, indentation: Int) {
    val indent = " " * indentation
    println(s"$indent$s")
  }

  def returnType(statement: NativePipeStatement): String =
    statement.returnType.prettyPrint

  def pos(positional: Positional): String =
    s"${positional.pos}"

  def prettyPrint(statement: NativePipeStatement, indentation: Int) {
    statement match {
      case s @ ControllerDefinition(identifier, _, _, _, _, _, _, _) =>
        printIndented(s"$identifier ${returnType(s)} ${pos(s)}", indentation)
      case s @ NativeFunctionApplication(identifier, arg1, arg2, _) =>
        printIndented(s"${identifier.name}(", indentation)
        prettyPrint(arg1, indentation + 2)
        prettyPrint(arg2, indentation + 2)
        printIndented(s") ${returnType(s)} ${pos(identifier)}", indentation)
      case s @ NativeIfStatement(cond, arg1, arg2) =>
        printIndented("if(", indentation)
        prettyPrint(cond, indentation + 2)
        prettyPrint(arg1, indentation + 2)
        prettyPrint(arg2, indentation + 2)
        printIndented(s") ${returnType(s)} ${pos(s)}", indentation)
      case c: Constant =>
        printIndented(s"${c.value} ${returnType(c)} ${pos(c)}", indentation)
      case _: Noop =>
        printIndented("noop", indentation)
    }
  }

  val NoopStatement = new Noop()
}

case class FunctionApplication(identifier: NSIdentifier, arguments: Seq[PipeStatement]) extends PipeStatement

case class IfStatement(cond: PipeStatement, `then`: PipeStatement, `else`: PipeStatement) extends PipeStatement

case class Value(identifier: String) extends PipeStatement

case class Constant(value: Int) extends PipeStatement with NativePipeStatement {
  def returnType = IntegerType(Seq(MinMax(value, value)))
}

case class NativeFunctionApplication(identifier: NSIdentifier,
                                     arg1: NativePipeStatement,
                                     arg2: NativePipeStatement,
                                     nf: NativeFunction)
    extends NativePipeStatement {
  def returnType = nf.returnType(arg1.returnType, arg2.returnType)
}

case class NativeIfStatement(cond: NativePipeStatement, `then`: NativePipeStatement, `else`: NativePipeStatement)
    extends NativePipeStatement {
  def returnType = Type.merge(`then`.returnType, `else`.returnType)
}

sealed trait Function {
  def signature: Seq[Type]
  def canBeAppliedTo(args: Seq[Type]): Boolean =
    args.size == signature.size && args.zip(signature).foldLeft(true) { (acc, e) =>
      acc & Type.withinBounds(e._1, e._2)
    }
}

case class FunctionDefinition(identifier: NSIdentifier,
                              arguments: Seq[String],
                              statement: PipeStatement,
                              signature: Seq[Type])
    extends Function
    with Positional

case class NativeFunction(signature: Seq[Type], returnType: (Type, Type) => Type) extends Function

case object Scale extends Function {
  override val signature: Seq[Type] = Seq(IntegerType(Seq(MinMax(Instruction.IntMin, Instruction.IntMax))))
}

case class ControllerDefinition(identifier: String,
                                groupIdentifier: String,
                                row: Int,
                                column: Int,
                                description: Text,
                                min: Int,
                                max: Int,
                                step: Int)
    extends NativePipeStatement {
  def returnType = IntegerType(Seq(MinMax(min, max)))
}

class Noop extends NativePipeStatement {
  override def returnType = IntegerType(Seq(MinMax(0, 0)))
}

case class MidiDefinition(midi: MidiMessageType, arguments: Seq[String], statement: PipeStatement) extends Positional

case class GroupDefinition(identifier: String,
                           description: Text,
                           rowFrom: Int,
                           columnFrom: Int,
                           rowTo: Int,
                           columnTo: Int)

object PipeParser extends Parsers {
  override type Elem = PipeToken

  def identifier = positioned(accept("identifier", { case id: Identifier => id }))

  def intnum = positioned(accept("integer", { case i: IntNum => i }))

  def text = positioned(accept("text", { case t: Text => t }))

  def newline = positioned(accept("newline", { case n: NewLine => n }))

  def `def` = positioned(accept("def", { case d: Def => d }))

  def `if` = positioned(accept("if", { case i: If => i }))

  def int = positioned(accept("int", { case i: IntToken => i }))

  def open = positioned(accept("(", { case o: Open => o }))

  def close = positioned(accept(")", { case c: Close => c }))

  def squareopen = positioned(accept("[", { case o: SquareOpen => o }))

  def squareclose = positioned(accept("]", { case c: SquareClose => c }))

  def comma = positioned(accept(",", { case c: Comma => c }))

  def equals = positioned(accept("=", { case e: Equals => e }))

  def tilde = positioned(accept("~", { case t: Tilde => t }))

  def minus = positioned(accept("-", { case m: Minus => m }))

  def controller = positioned(accept("controller", { case k: Controller => k }))

  def group = positioned(accept("group", { case g: Group => g }))

  def dot = positioned(accept(".", { case d: Dot => d }))

  def midicc = positioned(accept("midicc", { case m: MidiCCToken => m }))

  def midirpn = positioned(accept("midirpn", { case m: MidiRPNToken => m }))

  def midinrpn = positioned(accept("midinrpn", { case m: MidiNRPNToken => m }))

  def arglist(namespaceSeq: Seq[String]): Parser[Positioned[Seq[PipeStatement]]] =
    positioned(rep1sep(statement(namespaceSeq), comma) ^^ (Positioned(_)))

  def constant = positioned(
    minus.? ~ intnum ^^ {
      case Some(_) ~ i => Constant(i.value * -1)
      case None ~ i    => Constant(i.value)
    }
  )

  def namespacePart =
    positioned(identifier ~ dot ^^ {
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

  def intrange =
    constant ~ dot ~ dot ~ constant ^^ {
      case min ~ _ ~ _ ~ max => MinMax(min.value, max.value)
    }

  def intrangelist =
    squareopen ~ repsep(intrange, comma) ~ squareclose ^^ {
      case _ ~ ranges ~ _ => ranges
    }

  def inttype: Parser[Type] = positioned(
    int ~ intrangelist.? ^^ {
      case _ ~ Some(ranges) => IntegerType(ranges)
      case _ ~ None         => Instruction.FullIntRange
    }
  )

  def value = positioned(accept("identifier", { case i: Identifier => Value(i.name) }))

  def fndefarg =
    inttype ~ identifier ^^ {
      case t ~ i => (i, t)
    }

  def fndefarglist: Parser[(Seq[Identifier], Seq[Type])] = repsep(fndefarg, comma) ^^ { s: Seq[(Identifier, Type)] =>
    s.unzip
  }

  private def notInArgs(statement: PipeStatement, arguments: Seq[String], errorStr: String): Option[String] = {
    val missing = for {
      v <- PipeStatement.values(statement) if !arguments.contains(v.identifier)
    } yield {
      s"${v.identifier} is undefined in $errorStr"
    }
    if(missing.isEmpty) {
      None
    } else {
      Some(missing.mkString("\n"))
    }
  }

  def fnDef(namespaceSeq: Seq[String]) =
    positioned {
      `def` ~ namespaceIdentifier(namespaceSeq) ~ open ~ fndefarglist ~ close ~ equals ~ newline.* ~ statement(
        namespaceSeq) flatMap {
        case _ ~ i ~ _ ~ ((args, types)) ~ _ ~ _ ~ _ ~ statement =>
          val arguments = args.map(_.name)
          notInArgs(statement, arguments, s"function ${i.name}") match {
            case None => success(FunctionDefinition(i, arguments, statement, types))
            case Some(error) => err(error)
          }
      }
    }

  def controllerDef =
    positioned(
      controller ~ identifier ~ open ~ identifier ~ comma ~ constant ~ comma ~ constant ~ comma ~ text ~ comma ~ constant ~ comma ~ constant ~ comma ~ constant ~ close ^^ {
        case _ ~ i ~ _ ~ g ~ _ ~ row ~ _ ~ column ~ _ ~ description ~ _ ~ min ~ _ ~ max ~ _ ~ step ~ _ =>
          ControllerDefinition(i.name, g.name, row.value, column.value, description, min.value, max.value, step.value)
      })

  def groupDef =
    positioned(
      group ~ identifier ~ open ~ text ~ comma ~ constant ~ comma ~ constant ~ comma ~ constant ~ comma ~ constant ~ close ^^ {
        case _ ~ i ~ _ ~ description ~ _ ~ fromRow ~ _ ~ fromColumn ~ _ ~ toRow ~ _ ~ toColumn ~ _ =>
          Positioned(GroupDefinition(i.name, description, fromRow.value, fromColumn.value, toRow.value, toColumn.value))
      })

  def mididefarglist: Parser[Positioned[Seq[Identifier]]] = positioned(repsep(identifier, comma) ^^ (Positioned(_)))

  def midiCCDef(namespaceSeq: Seq[String]) =
    positioned {
      midicc ~ open ~ constant ~ close ~ open ~ constant ~ close ~ open ~ mididefarglist ~ close ~ equals ~ newline.* ~ statement(namespaceSeq) flatMap {
        case _ ~ _ ~ channel ~ _ ~ _ ~ cc ~ _ ~ _ ~ args ~ _ ~ _ ~ _ ~ statement =>
          val arguments = args.value.map(_.name)
          val midiCC = MidiCC(channel.value, cc.value)
          notInArgs(statement, arguments, s"definition of ${midiCC.identifier}") match {
            case None => success(MidiDefinition(midiCC, arguments, statement))
            case Some(error) => err(error)
          }
      }
    }

  def midiRPNDef(namespaceSeq: Seq[String]) =
    positioned {
      midirpn ~ open ~ constant ~ close ~ open ~ constant ~ comma ~ constant ~ close ~ open ~ mididefarglist ~ close ~ equals ~ newline.* ~ statement(namespaceSeq) flatMap {
        case _ ~ _ ~ channel ~ _ ~ _ ~ msb ~ _ ~ lsb ~ _ ~ _ ~ args ~ _ ~ _ ~ _ ~ statement =>
          val arguments = args.value.map(_.name)
          val midiRPN = MidiRPN(channel.value, msb.value, lsb.value)
          notInArgs(statement, arguments, s"definition of ${midiRPN.identifier}") match {
            case None => success(MidiDefinition(midiRPN, arguments, statement))
            case Some(error) => err(error)
          }
      }
    }

  def midiNRPNDef(namespaceSeq: Seq[String]) =
    positioned {
      midinrpn ~ open ~ constant ~ close ~ open ~ constant ~ comma ~ constant ~ close ~ open ~ mididefarglist ~ close ~ equals ~ newline.* ~ statement(namespaceSeq) flatMap {
        case _ ~ _ ~ channel ~ _ ~ _ ~ msb ~ _ ~ lsb ~ _ ~ _ ~ args ~ _ ~ _ ~ _ ~ statement =>
          val arguments = args.value.map(_.name)
          val midiNRPN = MidiNRPN(channel.value, msb.value, lsb.value)
          notInArgs(statement, arguments, s"definition of midi ${midiNRPN.identifier}") match {
            case None => success(MidiDefinition(midiNRPN, arguments, statement))
            case Some(error) => err(error)
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
    phrase(rep1(controllerDef | midiCCDef(namespaceSeq) | midiRPNDef(namespaceSeq) | midiNRPNDef(namespaceSeq) | fnDef(namespaceSeq) | groupDef | newline)) flatMap { defs =>
      val ccs = defs.collect {
        case cc: MidiDefinition => cc
      }

      val controllers = Map(defs.collect {
        case controller: ControllerDefinition => controller.identifier -> controller
      }: _*)

      val groups = Map(defs.collect {
        case Positioned(group: GroupDefinition) => group.identifier -> group
      }: _*)

      val functions = Map(defs.collect {
        case fn: FunctionDefinition => fn.identifier -> fn
      }: _*)

      val undefinedGroups = for {
        controller <- controllers.values if !groups.contains(controller.groupIdentifier)
      } yield {
        s"No such group ${controller.groupIdentifier} defined for controller ${controller.identifier}"
      }

      val undefinedControllers = for {
        cc <- ccs
        argument <- cc.arguments if !controllers.contains(argument)
      } yield {
        s"No such controller ${argument} defined for ${cc.midi.identifier}"
      }

      val undefined = undefinedGroups ++ undefinedControllers

      if (undefined.isEmpty) {
        success(PipeProgram(ccs, controllers, groups, functions))
      } else {
        err(undefined.mkString("\n"))
      }
    }

  def isDefined(fn: FunctionApplication, functions: Map[NSIdentifier, FunctionDefinition]): Boolean =
    !Instruction.nativeFunctions.contains(fn.identifier) && !functions.contains(fn.identifier)

  def parseTokens(tokens: List[PipeToken], moduleNs: Seq[String]): Either[Seq[CompilerError], PipeProgram] =
    file(moduleNs)(new PipeTokenReader(tokens)) match {
      case PipeParser.NoSuccess(msg, next) =>
        Left(Seq(CompilerError(next.pos, msg)))
      case PipeParser.Success(ast, next) =>
        Right(ast)
    }
}

class PipeTokenReader(tokens: Seq[PipeToken]) extends Reader[PipeToken] {
  override def first: PipeToken = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)
  override def rest: Reader[PipeToken] = new PipeTokenReader(tokens.tail)
}
