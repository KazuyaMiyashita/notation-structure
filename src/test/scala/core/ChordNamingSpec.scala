package core

import org.scalatest._
import ChordType._

class ChordNamingSpec extends WordSpec with Matchers {

  "root C" should {

    "name (C, E, G) to C Major" in {

      val notes = Set(Pitch(0, 0), Pitch(-2, 4), Pitch(0, 1))
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(ChordName(
        chordType = Major,
        rootNote = NoteName(0),
        baseNote = NoteName(0),
        tensions = Set()
      ))

      chordName shouldEqual result

    }

    "name (C, Eb, G) to C Minor" in {

      val notes = Set(Pitch(0, 0), Pitch(2, -3), Pitch(0, 1))
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(ChordName(
        chordType = Minor,
        rootNote = NoteName(0),
        baseNote = NoteName(0),
        tensions = Set()
      ))

      chordName shouldEqual result

    }

    "name (C, E, G, Bb) to C MajorSeventh" in {

      val notes = Set(Pitch(0, 0), Pitch(-2, 4), Pitch(0, 1), Pitch(2, -2))
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(ChordName(
        chordType = MajorSeventh,
        rootNote = NoteName(0),
        baseNote = NoteName(0),
        tensions = Set()
      ))

      chordName shouldEqual result

    }

    "name (C, Eb, G, Bb) to C MinorSeventh" in {

      val notes = Set(Pitch(0, 0), Pitch(2, -3), Pitch(0, 1), Pitch(2, -2))
      val chordName = ChordNaming.calculate(notes)
      
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

      val notes = Set(Pitch(-1,2), Pitch(-3,6), Pitch(-1,3))
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(ChordName(
        chordType = Major,
        rootNote = NoteName(2),
        baseNote = NoteName(2),
        tensions = Set()
      ))

      chordName shouldEqual result

    }

    "name (D, F, A) to D Minor" in {

      val notes = Set(Pitch(-1, 2), Pitch(1, -1), Pitch(-1, 3))
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(ChordName(
        chordType = Minor,
        rootNote = NoteName(2),
        baseNote = NoteName(2),
        tensions = Set()
      ))

      chordName shouldEqual result

    }

    "name (D, F#, A, C) to D MajorSeventh" in {

      val notes = Set(Pitch(-1, 2), Pitch(-3, 6), Pitch(-1, 3), Pitch(1, 0))
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(ChordName(
        chordType = MajorSeventh,
        rootNote = NoteName(2),
        baseNote = NoteName(2),
        tensions = Set()
      ))

      chordName shouldEqual result

    }

    "name (D, F, A, C) to D MinorSeventh" in {

      val notes = Set(Pitch(-1, 2), Pitch(1, -1), Pitch(-1, 3), Pitch(1, 0))
      val chordName = ChordNaming.calculate(notes)
      
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
