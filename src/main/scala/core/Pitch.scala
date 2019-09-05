package core

case class Pitch(octave: Int, fifth: FifthName) {

  def +(that: Pitch) = Pitch(this.octave + that.octave, this.fifth + that.fifth)
  def -(that: Pitch) = Pitch(this.octave - that.octave, this.fifth - that.fifth)

  def toMidiNoteNumber: MidiNoteNumber = {
    MidiNoteNumber(60 + (octave * 12) + (fifth.value * 7))
  }

}

object Pitch {
  def apply(octave: Int, fifthValue: Int) = new Pitch(octave, FifthName(fifthValue))
}
