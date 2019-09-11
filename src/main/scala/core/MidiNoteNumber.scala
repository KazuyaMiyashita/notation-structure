package core

case class MidiNoteNumber(value: Int) extends Ordered[MidiNoteNumber] {

  def compare(that: MidiNoteNumber): Int = this.value compare that.value

}
