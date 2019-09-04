package core

import org.scalatest._

class NoteSpec extends FlatSpec with Matchers {

  it should "convert note to midi note number" in {

    Note(0, 0).toMidiNoteNumber shouldEqual MidiNoteNumber(60)
    Note(1, 0).toMidiNoteNumber shouldEqual MidiNoteNumber(72)
    Note(0, 1).toMidiNoteNumber shouldEqual MidiNoteNumber(67)

  }

  it should "distinguish enharmonic" in {

    val cSharp = Note(-4, 7)
    val dFlat = Note(3, -5)

    cSharp.toMidiNoteNumber shouldEqual dFlat.toMidiNoteNumber
    (cSharp == dFlat) shouldEqual false

  }

}
