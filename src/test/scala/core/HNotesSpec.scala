package core

import org.scalatest._
import PitchName._

class HNotesSpec extends FlatSpec with Matchers {

  it should "convert scale to midi note number" in {

    val scale = HNotes.from(
      Note(C4, Duration(0)),
      Note(D4, Duration(0)),
      Note(E4, Duration(0)),
      Note(F4, Duration(0)),
      Note(G4, Duration(0)),
      Note(A4, Duration(0)),
      Note(B4, Duration(0)),
      Note(C5, Duration(0))
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
      Note(C4, Duration(0)),
      Note(D4, Duration(0)),
      Note(E4, Duration(0)),
      Note(F4, Duration(0)),
      Note(G4, Duration(0)),
      Note(A4, Duration(0)),
      Note(B4, Duration(0)),
      Note(C5, Duration(0))
    )

    val dMajorScale = HNotes.from(
      Note(D4, Duration(0)),
      Note(E4, Duration(0)),
      Note(Fs4, Duration(0)),
      Note(G4, Duration(0)),
      Note(A4, Duration(0)),
      Note(B4, Duration(0)),
      Note(Cs5, Duration(0)),
      Note(D5, Duration(0))
    )

    val cToD: Note = Note(D4 - C4, Duration(0))

    cMajorScale.transpose(cToD) shouldEqual dMajorScale

  }

}
