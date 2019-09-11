package core

case class MidiNoteNumber(value: Int) extends Ordering[MidiNoteNumber] {

  def compare(x: MidiNoteNumber, y: MidiNoteNumber): Int = x.value compare y.value

}
