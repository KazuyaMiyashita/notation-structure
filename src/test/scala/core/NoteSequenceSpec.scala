package core

import org.scalatest._

class NoteSequenceSpec extends FlatSpec with Matchers {

  it should "convert scale to midi note number" in {

    val scale = NoteSequence.from(
      Note(0, 0),
      Note(-1, 2),
      Note(-2, 4),
      Note(1, -1),
      Note(0, 1),
      Note(-1, 3),
      Note(-2, 5),
      Note(1, 0)
    )

    val midiNotes: Seq[MidiNoteNumber] = Seq(
      MidiNoteNumber(60),
      MidiNoteNumber(62),
      MidiNoteNumber(64),
      MidiNoteNumber(65),
      MidiNoteNumber(67),
      MidiNoteNumber(69),
      MidiNoteNumber(71),
      MidiNoteNumber(72),
    )

    scale.map(_.toMidiNoteNumber) shouldEqual midiNotes

  }

}
