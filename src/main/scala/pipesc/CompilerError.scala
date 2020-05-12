package pipesc

import java.nio.file.Path
import scala.util.parsing.input.Position

case class CompilerError(path: Path, position: Position, message: String) {

  def prettyPrint = s"$path:${position.line}:${position.column}: $message\n${position.longString}"
}
