package core

case class Pitch(octave: Int, fifth: Int) {

  def +(that: Pitch) = Pitch(this.octave + that.octave, this.fifth + that.fifth)
  def -(that: Pitch) = Pitch(this.octave - that.octave, this.fifth - that.fifth)

  def toMidiNoteNumber: MidiNoteNumber = {
    MidiNoteNumber(60 + (octave * 12) + (fifth * 7))
  }

}
