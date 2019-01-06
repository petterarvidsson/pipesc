package pipesc

object Predef {
  val NS = Seq(Identifier("predef"))
  val Add = Identifier("add")
  val Mul = Identifier("mul")
  val Sub = Identifier("sub")
  val Div = Identifier("div")
  val Mod = Identifier("mod")
  val is = Set(Add, Mul, Sub, Div, Mod)
}
