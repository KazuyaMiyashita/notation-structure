package core

case class Note(octave: Int, fifth: Int) {

  def toMidiNoteNumber: MidiNoteNumber = {
    MidiNoteNumber(60 + (octave * 12) + (fifth * 7))
  }

}
