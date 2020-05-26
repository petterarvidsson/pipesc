package pipesc

import java.nio.file.Path
import scala.util.parsing.input.Position

case class CompilerError(position: Position, message: String) {

  def prettyPrint(path: Path) = s"$path:${position.line}:${position.column}: $message\n${position.longString}"
}
