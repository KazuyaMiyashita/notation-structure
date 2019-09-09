package core

case class Interval(pitchOnC: Pitch) {
  def +(that: Interval) = Interval(this.pitchOnC + that.pitchOnC)
  def -(that: Interval) = Interval(this.pitchOnC - that.pitchOnC)

  final def on(fifthName: FifthName): FifthName = pitchOnC.fifth + fifthName
  final def on(pitch: Pitch): Pitch = pitchOnC + pitch
}

object Interval {
  import PitchName._

  object PerfectUnison extends Interval(C4)
  object AugumentedUnison extends Interval(Cs4)

  object DiminishedSecond extends Interval(Dbb4)
  object MinorSecond extends Interval(Db4)
  object MajorSecond extends Interval(D4)
  object AugumentedSecond extends Interval(Ds4)

  object DiminishedThird extends Interval(Ebb4)
  object MinorThird extends Interval(Eb4)
  object MajorThird extends Interval(E4)
  object AugumentedThird extends Interval(Es4)

  object DiminishedFourth extends Interval(Fb4)
  object PerfectFourth extends Interval(F4)
  object AugumentedFourth extends Interval(Fs4)

  object DiminishedFifth extends Interval(Gs4)
  object PerfectFifth extends Interval(G4)
  object AugumentedFifth extends Interval(Gs4)

  object DiminishedSixth extends Interval(Abb4)
  object MinorSixth extends Interval(Ab4)
  object MajorSixth extends Interval(A4)
  object AugumentedSixth extends Interval(As4)

  object DiminishedSeventh extends Interval(Bbb4)
  object MinorSeventh extends Interval(Bb4)
  object MajorSeventh extends Interval(B4)
  object AugumentedSeventh extends Interval(Bs4)

  object DiminishedOctave extends Interval(Cb5)
  object PerfectOctave extends Interval(C5)
  object AugumentedOctave extends Interval(Cs5)
}
