package core

case class Enharmonic(midiNoteNumber: MidiNoteNumber) {

  def pitchs: Set[Pitch] = {
    val fifths = -15 to 19
    ???
  }

}

object Enharmonic {

  def fromMidiNoteNumber(midiNoteNumber: MidiNoteNumber): Enharmonic = {
    new Enharmonic(midiNoteNumber)
  }

}
