package core

import org.scalatest._

class HNotesSpec extends FlatSpec with Matchers {

  it should "convert scale to midi note number" in {

    val scale = HNotes.from(
      Note(Pitch(0, 0), Duration(0)),
      Note(Pitch(-1, 2), Duration(0)),
      Note(Pitch(-2, 4), Duration(0)),
      Note(Pitch(1, -1), Duration(0)),
      Note(Pitch(0, 1), Duration(0)),
      Note(Pitch(-1, 3), Duration(0)),
      Note(Pitch(-2, 5), Duration(0)),
      Note(Pitch(1, 0), Duration(0))
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

    scale.map(_.pitch.toMidiNoteNumber) shouldEqual midiNotes

  }

  it should "transpose" in {

    val cMajorScale = HNotes.from(
      Note(Pitch(0, 0), Duration(0)),
      Note(Pitch(-1, 2), Duration(0)),
      Note(Pitch(-2, 4), Duration(0)),
      Note(Pitch(1, -1), Duration(0)),
      Note(Pitch(0, 1), Duration(0)),
      Note(Pitch(-1, 3), Duration(0)),
      Note(Pitch(-2, 5), Duration(0)),
      Note(Pitch(1, 0), Duration(0))
    )

    val dMajorScale = HNotes.from(
      Note(Pitch(-1, 2), Duration(0)),
      Note(Pitch(-2, 4), Duration(0)),
      Note(Pitch(-3, 6), Duration(0)),
      Note(Pitch(0, 1), Duration(0)),
      Note(Pitch(-1, 3), Duration(0)),
      Note(Pitch(-2, 5), Duration(0)),
      Note(Pitch(-3, 7), Duration(0)),
      Note(Pitch(0, 2), Duration(0))
    )

    val cToD: Note = Note(Pitch(-1, 2) - Pitch(0, 0), Duration(0))

    cMajorScale.transpose(cToD) shouldEqual dMajorScale

  }

}
