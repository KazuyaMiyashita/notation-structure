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

    "name (C, E, G, Bb) to C Seventh" in {
      val notes = Set(C4, E4, G4, Bb4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(C, Seventh))

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

    "name (D, F#, A, C) to D Seventh" in {
      val notes = Set(D4, Fs4, A4, C5)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(D, Seventh))

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

    "name (C, D, E, G, Bb) to C Seventh AddNinth" in {
      val notes = Set(C4, D4, E4, G4, Bb4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(C, Seventh).withTensions(AddNinth))

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

    "name (D, E, F#, A, C) to D Seventh AddNinth" in {
      val notes = Set(D4, E4, Fs4, A4, C5)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(D, Seventh).withTensions(AddNinth))

      chordName shouldEqual result
    }

    "name (D, E, F, A, C) to D MinorSeventh AddNinth" in {
      val notes = Set(D4, E4, F4, A4, C5)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(D, MinorSeventh).withTensions(AddNinth))

      chordName shouldEqual result
    }

  }

  "root C with bass" should {

    "name (C, E, G) / E to C Major / E" in {
      val notes = Set(C4, E4, G4, E3)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(C, Major).withBass(E))

      chordName shouldEqual result
    }

    "name (C, E, G) / G to C Major / G" in {
      val notes = Set(C4, E4, G4, G3)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(C, Major).withBass(G))

      chordName shouldEqual result
    }

    "name (C, Eb, G) / Eb to C Minor / Eb" in {
      val notes = Set(C4, Eb4, G4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(C, Minor).withBass(Eb))

      chordName shouldEqual result
    }

    "name (C, Eb, G) / G to C Minor / G" in {
      val notes = Set(C4, Eb4, G4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(C, Minor).withBass(G))

      chordName shouldEqual result
    }

    "name (C, E, G, Bb) / E to C Seventh / E" in {
      val notes = Set(C4, E4, G4, Bb4, E3)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(C, Seventh).withBass(E))

      chordName shouldEqual result
    }

    "name (C, E, G, Bb) / G to C Seventh / G" in {
      val notes = Set(C4, E4, G4, Bb4, G3)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(C, Seventh).withBass(G))

      chordName shouldEqual result
    }

    "name (C, E, G, Bb) / Bb to C Seventh / Bb" in {
      val notes = Set(C4, E4, G4, Bb4, Bb3)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(C, Seventh).withBass(Bb))

      chordName shouldEqual result
    }

    "name (C, Eb, G, Bb) / Eb to C MinorSeventh / Eb" in {
      val notes = Set(C4, Eb4, G4, Bb4, Eb3)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(C, MinorSeventh).withBass(Eb))

      chordName shouldEqual result
    }

    "name (C, Eb, G, Bb) / G to C MinorSeventh / G" in {
      val notes = Set(C4, Eb4, G4, Bb4, G3)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(C, MinorSeventh).withBass(G))

      chordName shouldEqual result
    }

    "name (C, Eb, G, Bb) / Bb to C MinorSeventh / Bb" in {
      val notes = Set(C4, Eb4, G4, Bb4, Bb3)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(C, MinorSeventh).withBass(Bb))

      chordName shouldEqual result
    }

  }

  "root D with bass" should {

    "name (D, F#, A) / F# to D Major / F#" in {
      val notes = Set(D4, Fs4, A4, Fs3)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(D, Major).withBass(Fs))

      chordName shouldEqual result
    }

    "name (D, F#, A) / A to D Major / A" in {
      val notes = Set(D4, Fs4, A4, A3)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(D, Major).withBass(A))

      chordName shouldEqual result
    }

    "name (D, F, A) / F to D Minor / F" in {
      val notes = Set(D4, F4, A4, F3)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(D, Minor).withBass(F))

      chordName shouldEqual result
    }

    "name (D, F, A) / A to D Minor / A" in {
      val notes = Set(D4, F4, A4, A3)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(D, Minor).withBass(A))

      chordName shouldEqual result
    }

    "name (D, F#, A, C) / F# to D Seventh / F#" in {
      val notes = Set(D4, Fs4, A4, C5, Fs3)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(D, Seventh).withBass(Fs))

      chordName shouldEqual result
    }

    "name (D, F#, A, C) / A to D Seventh / A" in {
      val notes = Set(D4, Fs4, A4, C5, A3)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(D, Seventh).withBass(A))

      chordName shouldEqual result
    }

    "name (D, F#, A, C) / C to D Seventh / C" in {
      val notes = Set(D4, Fs4, A4, C5, C4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(D, Seventh).withBass(C))

      chordName shouldEqual result
    }

    "name (D, F, A, C) / F to D MinorSeventh / F" in {
      val notes = Set(D4, F4, A4, C5, F3)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(D, MinorSeventh).withBass(F))

      chordName shouldEqual result
    }

    "name (D, F, A, C) / A to D MinorSeventh / A" in {
      val notes = Set(D4, F4, A4, C5, A3)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(D, MinorSeventh).withBass(A))

      chordName shouldEqual result
    }

    "name (D, F, A, C) / C to D MinorSeventh / C" in {
      val notes = Set(D4, F4, A4, C5, C4)
      val chordName = ChordNaming.calculate(notes)
      
      val result = Right(Chord(D, MinorSeventh).withBass(C))

      chordName shouldEqual result
    }

  }


}
