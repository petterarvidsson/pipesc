package pipesc

sealed trait MidiMessageType {
  def identifier: String
}
case class MidiCC(cc: Int) extends MidiMessageType {
  def identifier: String = s"midi_cc($cc)"
}
case class MidiRPN(msb: Int, lsb: Int) extends MidiMessageType {
  def identifier: String = s"midi_rpn($lsb,$msb)"
}
case class MidiNRPN(msb: Int, lsb: Int) extends MidiMessageType {
  def identifier: String = s"midi_nrpn($lsb,$msb)"
}
