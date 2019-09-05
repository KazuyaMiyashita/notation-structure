package core

import org.scalatest._
import ChordType._
import FifthName._
import PitchName._

class ChordNamingSpec extends WordSpec with Matchers {

  "root C" should {

    "name (C, E, G) to C Major" in {

      val notes = Set(C4, E4, G4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(
        chordType = Major,
        rootNote = C,
        baseNote = C,
        tensions = Set()
      ))

      chordName shouldEqual result

    }

    "name (C, Eb, G) to C Minor" in {

      val notes = Set(C4, Eb4, G4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(
        chordType = Minor,
        rootNote = C,
        baseNote = C,
        tensions = Set()
      ))

      chordName shouldEqual result

    }

    "name (C, E, G, Bb) to C MajorSeventh" in {

      val notes = Set(C4, E4, G4, Bb4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(
        chordType = MajorSeventh,
        rootNote = C,
        baseNote = C,
        tensions = Set()
      ))

      chordName shouldEqual result

    }

    "name (C, Eb, G, Bb) to C MinorSeventh" in {

      val notes = Set(C4, Eb4, G4, Bb4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(
        chordType = MinorSeventh,
        rootNote = C,
        baseNote = C,
        tensions = Set()
      ))

      chordName shouldEqual result

    }

  }

  "root D" should {

    "name (D, F#, A) to D Major" in {

      val notes = Set(D4, Fs4, A4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(
        chordType = Major,
        rootNote = D,
        baseNote = D,
        tensions = Set()
      ))

      chordName shouldEqual result

    }

    "name (D, F, A) to D Minor" in {

      val notes = Set(D4, F4, A4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(
        chordType = Minor,
        rootNote = D,
        baseNote = D,
        tensions = Set()
      ))

      chordName shouldEqual result

    }

    "name (D, F#, A, C) to D MajorSeventh" in {

      val notes = Set(D4, Fs4, A4, C5)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(
        chordType = MajorSeventh,
        rootNote = D,
        baseNote = D,
        tensions = Set()
      ))

      chordName shouldEqual result

    }

    "name (D, F, A, C) to D MinorSeventh" in {

      val notes = Set(D4, F4, A4, C5)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(
        chordType = MinorSeventh,
        rootNote = D,
        baseNote = D,
        tensions = Set()
      ))

      chordName shouldEqual result

    }

  }


}
