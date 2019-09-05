package core

import org.scalatest._

class NoteSpec extends FlatSpec with Matchers {

  it should "convert note to midi note number" in {

    Pitch(0, 0).toMidiNoteNumber shouldEqual MidiNoteNumber(60)
    Pitch(1, 0).toMidiNoteNumber shouldEqual MidiNoteNumber(72)
    Pitch(0, 1).toMidiNoteNumber shouldEqual MidiNoteNumber(67)

  }

  it should "distinguish enharmonic" in {

    val cSharp = Pitch(-4, 7)
    val dFlat = Pitch(3, -5)

    cSharp.toMidiNoteNumber shouldEqual dFlat.toMidiNoteNumber
    (cSharp == dFlat) shouldEqual false

  }

  it should "add" in {
    Pitch(1, 0) + Pitch(-1, 2) shouldEqual Pitch(0, 2)
  }

  it should "subtract" in {
    Pitch(1, 0) - Pitch(-1, 2) shouldEqual Pitch(2, -2)
  }

}
