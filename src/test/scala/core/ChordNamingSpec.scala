package core

import org.scalatest._
import ChordType._

class ChordNamingSpec extends WordSpec with Matchers {

  "root C" should {

    "name (C, E, G) to C Major" in {

      val notes = Set(Note(0, 0), Note(-2, 4), Note(0, 1))
      val base = Note(-1, 0)
      val chordName = ChordNaming.judge(notes, base)
      
      val result = Right(ChordName(
        chordType = Major,
        rootNote = NoteName(0),
        baseNote = NoteName(0),
        tensions = Set()
      ))

      chordName shouldEqual result

    }

    "name (C, Eb, G) to C Minor" in {

      val notes = Set(Note(0, 0), Note(2, -3), Note(0, 1))
      val base = Note(-1, 0)
      val chordName = ChordNaming.judge(notes, base)
      
      val result = Right(ChordName(
        chordType = Minor,
        rootNote = NoteName(0),
        baseNote = NoteName(0),
        tensions = Set()
      ))

      chordName shouldEqual result

    }

    "name (C, E, G, Bb) to C MajorSeventh" in {

      val notes = Set(Note(0, 0), Note(-2, 4), Note(0, 1), Note(2, -2))
      val base = Note(-1, 0)
      val chordName = ChordNaming.judge(notes, base)
      
      val result = Right(ChordName(
        chordType = MajorSeventh,
        rootNote = NoteName(0),
        baseNote = NoteName(0),
        tensions = Set()
      ))

      chordName shouldEqual result

    }

    "name (C, Eb, G, Bb) to C MinorSeventh" in {

      val notes = Set(Note(0, 0), Note(2, -3), Note(0, 1), Note(2, -2))
      val base = Note(-1, 0)
      val chordName = ChordNaming.judge(notes, base)
      
      val result = Right(ChordName(
        chordType = MinorSeventh,
        rootNote = NoteName(0),
        baseNote = NoteName(0),
        tensions = Set()
      ))

      chordName shouldEqual result

    }

  }

  "root D" should {

    "name (D, F#, A) to D Major" in {

      val notes = Set(Note(-1,2), Note(-3,6), Note(-1,3))
      val base = Note(-2, 2)
      val chordName = ChordNaming.judge(notes, base)
      
      val result = Right(ChordName(
        chordType = Major,
        rootNote = NoteName(2),
        baseNote = NoteName(2),
        tensions = Set()
      ))

      chordName shouldEqual result

    }

    "name (D, F, A) to D Minor" in {

      val notes = Set(Note(-1, 2), Note(1, -1), Note(-1, 3))
      val base = Note(-2, 2)
      val chordName = ChordNaming.judge(notes, base)
      
      val result = Right(ChordName(
        chordType = Minor,
        rootNote = NoteName(2),
        baseNote = NoteName(2),
        tensions = Set()
      ))

      chordName shouldEqual result

    }

    "name (D, F#, A, C) to D MajorSeventh" in {

      val notes = Set(Note(-1, 2), Note(-3, 6), Note(-1, 3), Note(1, 0))
      val base = Note(-2, 2)
      val chordName = ChordNaming.judge(notes, base)
      
      val result = Right(ChordName(
        chordType = MajorSeventh,
        rootNote = NoteName(2),
        baseNote = NoteName(2),
        tensions = Set()
      ))

      chordName shouldEqual result

    }

    "name (D, F, A, C) to D MinorSeventh" in {

      val notes = Set(Note(-1, 2), Note(1, -1), Note(-1, 3), Note(1, 0))
      val base = Note(-2, 2)
      val chordName = ChordNaming.judge(notes, base)
      
      val result = Right(ChordName(
        chordType = MinorSeventh,
        rootNote = NoteName(2),
        baseNote = NoteName(2),
        tensions = Set()
      ))

      chordName shouldEqual result

    }

  }


}
