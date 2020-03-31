package pipesc

object Predef {
  val NS = Seq("predef")
  val Add = "add"
  val Mul = "mul"
  val Sub = "sub"
  val Div = "div"
  val Mod = "mod"
  val If = "if"
  val IfWithNs = NSIdentifier(NS, If)
  val is = Set(Add, Mul, Sub, Div, Mod, If)
}
