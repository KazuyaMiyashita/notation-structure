package core

import org.scalatest._

class HNotesSpec extends FlatSpec with Matchers {

  it should "convert scale to midi note number" in {

    val scale = HNotes.from(
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

  it should "transpose" in {

    val cMajorScale = HNotes.from(
      Note(0, 0),
      Note(-1, 2),
      Note(-2, 4),
      Note(1, -1),
      Note(0, 1),
      Note(-1, 3),
      Note(-2, 5),
      Note(1, 0)
    )

    val dMajorScale = HNotes.from(
      Note(-1,2),
      Note(-2,4),
      Note(-3,6),
      Note(0,1),
      Note(-1,3),
      Note(-2,5),
      Note(-3,7),
      Note(0,2)
    )

    val cToD: Note = Note(-1, 2) - Note(0, 0)

    cMajorScale.transpose(cToD) shouldEqual dMajorScale

  }

}
