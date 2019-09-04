package core

case class Note(octave: Int, fifth: Int) {

  def +(that: Note) = Note(this.octave + that.octave, this.fifth + that.fifth)
  def -(that: Note) = Note(this.octave - that.octave, this.fifth - that.fifth)

  def toMidiNoteNumber: MidiNoteNumber = {
    MidiNoteNumber(60 + (octave * 12) + (fifth * 7))
  }

}
