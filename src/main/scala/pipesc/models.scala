package pipesc

sealed trait MidiMessageType {
  def identifier: String
}
case class MidiCC(channel: Int, cc: Int) extends MidiMessageType {
  def identifier: String = s"midi_cc($channel)($cc)"
}
case class MidiRPN(channel: Int, msb: Int, lsb: Int) extends MidiMessageType {
  def identifier: String = s"midi_rpn($channel)($lsb,$msb)"
}
case class MidiNRPN(channel: Int, msb: Int, lsb: Int) extends MidiMessageType {
  def identifier: String = s"midi_nrpn($channel)($lsb,$msb)"
}
