package core

import org.scalatest._

class ChordSpec extends FlatSpec with Matchers {

  it should "constract from vertival notes(size >= 2)" in {

    val vnotes = VerticalNotes.from(
      Note(0, 0),
      Note(-2, 4),
      Note(0, 1),
    )
    val chordOption: Option[Chord] = Chord.fromVerticalNotes(vnotes)

    val result = Some(Chord(Set(Note(0, 0), Note(-2, 4), Note(0, 1)), Note(0, 0)))

    chordOption shouldEqual result

  }

  it should "constract from vertival notes(size == 1)" in {

    val vnotes = VerticalNotes.from(Note(0, 0))
    val chordOption: Option[Chord] = Chord.fromVerticalNotes(vnotes)

    val result = Some(Chord(Set(Note(0, 0)), Note(0, 0)))

    chordOption shouldEqual result

  }

  it should "constract from vertival notes(size == 0)" in {

    val vnotes = VerticalNotes.from()
    val chordOption: Option[Chord] = Chord.fromVerticalNotes(vnotes)

    val result = None

    chordOption shouldEqual result

  }

}
