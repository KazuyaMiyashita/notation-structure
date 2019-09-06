package core

import org.scalatest._
import ChordType._
import FifthName._
import PitchName._
import Tension._

class ChordNamingSpec extends WordSpec with Matchers {

  def nameof(ps: Pitch*) = ChordNaming.calculate(ps.toSet)

  "root C" should {

    "(C4, E4, G4) to C" in {
      nameof(C4, E4, G4) shouldEqual Right(Chord(C, Major))
    }

    "(C4, Eb4, G4) to Cm" in {
      nameof(C4, Eb4, G4) shouldEqual Right(Chord(C, Minor))
    }

    "(C4, E4, G4, Bb4) to C7" in {
      nameof(C4, E4, G4, Bb4) shouldEqual Right(Chord(C, Seventh))
    }

    "(C4, Eb4, G4, Bb4) to Cm7" in {
      nameof(C4, Eb4, G4, Bb4) shouldEqual Right(Chord(C, MinorSeventh))
    }

    "(C4, E4, G4, B4) to CM7" in {
      nameof(C4, E4, G4, B4) shouldEqual Right(Chord(C, MajorSeventh))
    }

  }

  "root D" should {

    "(D4, Fs4, A4) to D" in {
      nameof(D4, Fs4, A4) shouldEqual Right(Chord(D, Major))
    }

    "(D4, F4, A4) to Dm" in {
      nameof(D4, F4, A4) shouldEqual Right(Chord(D, Minor))
    }

    "(D4, Fs4, A4, C5) to D7" in {
      nameof(D4, Fs4, A4, C5) shouldEqual Right(Chord(D, Seventh))
    }

    "(D4, F4, A4, C5) to Dm7" in {
      nameof(D4, F4, A4, C5) shouldEqual Right(Chord(D, MinorSeventh))
    }

    "(D4, Fs4, A4, Cs5) to DM7" in {
      nameof(D4, Fs4, A4, Cs5) shouldEqual Right(Chord(D, MajorSeventh))
    }

  }

  "root C add9" should {

    "(C4, D4, E4, G4) to C add9" in {
      nameof(C4, D4, E4, G4) shouldEqual Right(Chord(C, Major).withTensions(Ninth))
    }

    "(C4, D4, Eb4, G4) to Cm add9" in {
      nameof(C4, D4, Eb4, G4) shouldEqual Right(Chord(C, Minor).withTensions(Ninth))
    }

    "(C4, D4, E4, G4, Bb4) to C7 add9" in {
      nameof(C4, D4, E4, G4, Bb4) shouldEqual Right(Chord(C, Seventh).withTensions(Ninth))
    }

    "(C4, D4, Eb4, G4, Bb4) to Cm7 add9" is (pending)
    // NOTE: The current situation is
    "(C4, D4, Eb4, G4, Bb4) to Cm7add9 or EbM7/C (currently)" in {
      nameof(C4, D4, Eb4, G4, Bb4) shouldEqual Left(Set(
        Chord(C, MinorSeventh).withTensions(Ninth),
        Chord(Eb, MajorSeventh).withBass(C)))
    }
    // NOTE: But I want to be like this
    // "(C4, D4, Eb4, G4, Bb4) to Cm7 add9" in {
    //   nameof(C4, D4, Eb4, G4, Bb4) shouldEqual Right(Chord(C, MinorSeventh).withTensions(Ninth))
    // }

    "(C4, D4, E4, G4, B4) to CM7 add9" is (pending)
    // NOTE: The current situation is
    "(C4, D4, E4, G4, B4) to CM7add9 or Em7/C (currently)" in {
      nameof(C4, D4, E4, G4, B4) shouldEqual Left(Set(
        Chord(C, MajorSeventh).withTensions(Ninth),
        Chord(E, MinorSeventh).withBass(C)))
    }
    // NOTE: But I want to be like this
    // "(C4, D4, E4, G4, B4) to CM7 add9" in {
    //   nameof(C4, D4, E4, G4, B4) shouldEqual Right(Chord(C, MajorSeventh).withTensions(Ninth))
    // }

    "(C4, E4, G4, Bb4, Db5) to C7(b9)" in {
      nameof(C4, D4, E4, G4, Bb4, Db5) shouldEqual Right(Chord(C, Seventh).withTensions(FlatNinth))
    }

    "(C4, E4, G4, Bb4, Ds5) to C7(#9)" in {
      nameof(C4, D4, E4, G4, Bb4, Ds5) shouldEqual Right(Chord(C, Seventh).withTensions(SharpNinth))
    }

  }

  "root D add9" should {

    "(D4, E4, Fs4, A4) to D add9" in {
      nameof(D4, E4, Fs4, A4) shouldEqual Right(Chord(D, Major).withTensions(Ninth))
    }

    "(D4, E4, F4, A4) to Dm add9" in {
      nameof(D4, E4, F4, A4) shouldEqual Right(Chord(D, Minor).withTensions(Ninth))
    }

    "(D4, E4, Fs4, A4, C5) to D7 add9" in {
      nameof(D4, E4, Fs4, A4, C5) shouldEqual Right(Chord(D, Seventh).withTensions(Ninth))
    }

    "(D4, E4, F4, A4, C5) to Dm7 add9" is (pending)
    // NOTE: The current situation is
    "(D4, E4, F4, A4, C5) to Dm7add9 or FM7/D (currently)" in {
      nameof(D4, E4, F4, A4, C5) shouldEqual Left(Set(
        Chord(D, MinorSeventh).withTensions(Ninth),
        Chord(F, MajorSeventh).withBass(D)))
    }
    // NOTE: But I want to be like this
    // "(D4, E4, F4, A4, C5) to Dm7 add9" in {
    //   nameof(D4, E4, F4, A4, C5) shouldEqual Right(Chord(D, MinorSeventh).withTensions(Ninth))
    // }

    "(D4, E4, Fs4, A4, Cs5) to DM7 add9" is (pending)
    // NOTE: The current situation is
    "(D4, E4, Fs4, A4, Cs5) to DM7add9 or F#m7/D (currently)" in {
      nameof(D4, E4, Fs4, A4, Cs5) shouldEqual Left(Set(
        Chord(D, MajorSeventh).withTensions(Ninth),
        Chord(Fs, MinorSeventh).withBass(D)))
    }
    // NOTE: But I want to be like this
    // "(D4, E4, Fs4, A4, Cs5) to DM7 add9" in {
    //   nameof(D4, E4, Fs4, A4, Cs5) shouldEqual Right(Chord(D, MajorSeventh).withTensions(Ninth))
    // }

    "(D4, Fs4, A4, C5, Eb5) to D7(b9)" in {
      nameof(D4, Fs4, A4, C5, Eb5) shouldEqual Right(Chord(D, Seventh).withTensions(FlatNinth))
    }

    "(D4, Fs4, A4, C5, Es5) to D7(#9)" in {
      nameof(D4, Fs4, A4, C5, Es5) shouldEqual Right(Chord(D, Seventh).withTensions(SharpNinth))
    }

  }

  "root C with bass" should {

    "(C4, E4, G4, E3) to C / E" in {
      nameof(C4, E4, G4, E3) shouldEqual Right(Chord(C, Major).withBass(E))
    }

    "(C4, E4, G4, G3) to C / G" in {
      nameof(C4, E4, G4, G3) shouldEqual Right(Chord(C, Major).withBass(G))
    }

    "(C4, Eb4, G4, Eb3) to Cm / Eb" in {
      nameof(C4, Eb4, G4, Eb3) shouldEqual Right(Chord(C, Minor).withBass(Eb))
    }

    "(C4, Eb4, G4, G3) to Cm / G" in {
      nameof(C4, Eb4, G4, G3) shouldEqual Right(Chord(C, Minor).withBass(G))
    }

    "(C4, E4, G4, Bb4, E3) to C7 / E" in {
      nameof(C4, E4, G4, Bb4, E3) shouldEqual Right(Chord(C, Seventh).withBass(E))
    }

    "(C4, E4, G4, Bb4, G3) to C7 / G" in {
      nameof(C4, E4, G4, Bb4, G3) shouldEqual Right(Chord(C, Seventh).withBass(G))
    }

    "(C4, E4, G4, Bb4, Bb3) to C7 / Bb" in {
      nameof(C4, E4, G4, Bb4, Bb3) shouldEqual Right(Chord(C, Seventh).withBass(Bb))
    }

    "(C4, Eb4, G4, Bb4, Eb3) to Cm7 / Eb" in {
      nameof(C4, Eb4, G4, Bb4, Eb3) shouldEqual Right(Chord(C, MinorSeventh).withBass(Eb))
    }

    "(C4, Eb4, G4, Bb4, G3) to Cm7 / G" in {
      nameof(C4, Eb4, G4, Bb4, G3) shouldEqual Right(Chord(C, MinorSeventh).withBass(G))
    }

    "(C4, Eb4, G4, Bb4, Bb3) to Cm7 / Bb" in {
      nameof(C4, Eb4, G4, Bb4, Bb3) shouldEqual Right(Chord(C, MinorSeventh).withBass(Bb))
    }

    "(C4, E4, G4, B4, E3) to CM7 / E" in {
      nameof(C4, E4, G4, B4, E3) shouldEqual Right(Chord(C, MajorSeventh).withBass(E))
    }

    "(C4, E4, G4, B4, E3) to CM7 / G" in {
      nameof(C4, E4, G4, B4, G3) shouldEqual Right(Chord(C, MajorSeventh).withBass(G))
    }

    "(C4, E4, G4, B4, E3) to CM7 / B" in {
      nameof(C4, E4, G4, B4, B3) shouldEqual Right(Chord(C, MajorSeventh).withBass(B))
    }

  }

  "root D with bass" should {

    "(D4, Fs4, A4, Fs3) to D / F#" in {
      nameof(D4, Fs4, A4, Fs3) shouldEqual Right(Chord(D, Major).withBass(Fs))
    }

    "(D4, Fs4, A4, A3) to D / A" in {
      nameof(D4, Fs4, A4, A3) shouldEqual Right(Chord(D, Major).withBass(A))
    }

    "(D4, F4, A4, F3) to Dm / F" in {
      nameof(D4, F4, A4, F3) shouldEqual Right(Chord(D, Minor).withBass(F))
    }

    "(D4, F4, A4, A3) to Dm / A" in {
      nameof(D4, F4, A4, A3) shouldEqual Right(Chord(D, Minor).withBass(A))
    }

    "(D4, Fs4, A4, C5, Fs3) to D7 / F#" in {
      nameof(D4, Fs4, A4, C5, Fs3) shouldEqual Right(Chord(D, Seventh).withBass(Fs))
    }

    "(D4, Fs4, A4, C5, A3) to D7 / A" in {
      nameof(D4, Fs4, A4, C5, A3) shouldEqual Right(Chord(D, Seventh).withBass(A))
    }

    "(D4, Fs4, A4, C5, C4) to D7 / C" in {
      nameof(D4, Fs4, A4, C5, C4) shouldEqual Right(Chord(D, Seventh).withBass(C))
    }

    "(D4, F4, A4, C5, F3) to Dm7 / F" in {
      nameof(D4, F4, A4, C5, F3) shouldEqual Right(Chord(D, MinorSeventh).withBass(F))
    }

    "(D4, F4, A4, C5, A3) to Dm7 / A" in {
      nameof(D4, F4, A4, C5, A3) shouldEqual Right(Chord(D, MinorSeventh).withBass(A))
    }

    "(D4, F4, A4, C5, C4) to Dm7 / C" in {
      nameof(D4, F4, A4, C5, C4) shouldEqual Right(Chord(D, MinorSeventh).withBass(C))
    }

    "(D4, Fs4, A4, CM5, Fs3) to DM7 / F#" in {
      nameof(D4, Fs4, A4, Cs5, Fs3) shouldEqual Right(Chord(D, MajorSeventh).withBass(Fs))
    }

    "(D4, Fs4, A4, CM5, A3) to DM7 / A" in {
      nameof(D4, Fs4, A4, Cs5, A3) shouldEqual Right(Chord(D, MajorSeventh).withBass(A))
    }

    "(D4, Fs4, A4, CM5, Cs4) to DM7 / C#" in {
      nameof(D4, Fs4, A4, Cs5, Cs4) shouldEqual Right(Chord(D, MajorSeventh).withBass(Cs))
    }

  }


}
