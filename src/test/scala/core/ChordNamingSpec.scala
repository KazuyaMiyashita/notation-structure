package core

import org.scalatest._
import ChordType._
import FifthName._
import PitchName._
import Tension._

class ChordNamingSpec extends WordSpec with Matchers {

  "root C" should {

    "name (C, E, G) to C Major" in {
      val notes = Set(C4, E4, G4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(C, Major))

      chordName shouldEqual result
    }

    "name (C, Eb, G) to C Minor" in {
      val notes = Set(C4, Eb4, G4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(C, Minor))

      chordName shouldEqual result
    }

    "name (C, E, G, Bb) to C MajorSeventh" in {
      val notes = Set(C4, E4, G4, Bb4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(C, MajorSeventh))

      chordName shouldEqual result
    }

    "name (C, Eb, G, Bb) to C MinorSeventh" in {
      val notes = Set(C4, Eb4, G4, Bb4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(C, MinorSeventh))

      chordName shouldEqual result
    }

  }

  "root D" should {

    "name (D, F#, A) to D Major" in {
      val notes = Set(D4, Fs4, A4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(D, Major))

      chordName shouldEqual result
    }

    "name (D, F, A) to D Minor" in {
      val notes = Set(D4, F4, A4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(D, Minor))

      chordName shouldEqual result
    }

    "name (D, F#, A, C) to D MajorSeventh" in {
      val notes = Set(D4, Fs4, A4, C5)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(D, MajorSeventh))

      chordName shouldEqual result
    }

    "name (D, F, A, C) to D MinorSeventh" in {
      val notes = Set(D4, F4, A4, C5)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(D, MinorSeventh))

      chordName shouldEqual result
    }

  }

  "root C AddNinth" should {

    "name (C, D, E, G) to C Major AddNinth" in {
      val notes = Set(C4, D4, E4, G4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(C, Major).withTensions(AddNinth))

      chordName shouldEqual result
    }

    "name (C, D, Eb, G) to C Minor AddNinth" in {
      val notes = Set(C4, D4, Eb4, G4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(C, Minor).withTensions(AddNinth))

      chordName shouldEqual result
    }

    "name (C, D, E, G, Bb) to C MajorSeventh AddNinth" in {
      val notes = Set(C4, D4, E4, G4, Bb4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(C, MajorSeventh).withTensions(AddNinth))

      chordName shouldEqual result
    }

    "name (C, D, Eb, G, Bb) to C MinorSeventh AddNinth" in {
      val notes = Set(C4, D4, Eb4, G4, Bb4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(C, MinorSeventh).withTensions(AddNinth))

      chordName shouldEqual result
    }

  }

  "root D AddNinth" should {

    "name (D, E, F#, A) to D Major AddNinth" in {
      val notes = Set(D4, E4, Fs4, A4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(D, Major).withTensions(AddNinth))

      chordName shouldEqual result
    }

    "name (D, E, F, A) to D Minor AddNinth" in {
      val notes = Set(D4, E4, F4, A4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(D, Minor).withTensions(AddNinth))

      chordName shouldEqual result
    }

    "name (D, E, F#, A, C) to D MajorSeventh AddNinth" in {
      val notes = Set(D4, E4, Fs4, A4, C5)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(D, MajorSeventh).withTensions(AddNinth))

      chordName shouldEqual result
    }

    "name (D, E, F, A, C) to D MinorSeventh AddNinth" in {
      val notes = Set(D4, E4, F4, A4, C5)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(D, MinorSeventh).withTensions(AddNinth))

      chordName shouldEqual result
    }

  }


}
