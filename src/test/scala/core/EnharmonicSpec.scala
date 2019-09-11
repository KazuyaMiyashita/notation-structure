package core

import org.scalatest._
import PitchName._

class EnharmonicSpec extends WordSpec with Matchers {

  def enharmonic(midiNum: Int): Enharmonic = Enharmonic(MidiNoteNumber(midiNum))
  def conbinations(midiNums: Int*): Set[Set[Pitch]] = {
    Enharmonic.combinate(midiNums.toSet.map((n: Int) => Enharmonic(MidiNoteNumber(n))))
  }

  "def pitchs" should {

    "60 to (Bs3, C4, Dbb4)" in {
      enharmonic(60).pitchSet shouldEqual Set(Bs3, C4, Dbb4)
    }

    "62 to (Css4, D4, Ebb4)" in {
      enharmonic(62).pitchSet shouldEqual Set(Css4, D4, Ebb4)
    }

    "68 to (Gs4, Ab4)" in {
      enharmonic(68).pitchSet shouldEqual Set(Gs4, Ab4)
    }

  }

  "def combinate" should {

    "conbinations(60, 63)" in {
      conbinations(60, 63) shouldEqual Set(
        Set(Bs3, Ds4),
        Set(Bs3, Eb4),
        // Set(Bs3, Fbb4), // ? 重減五度
        Set(C4, Ds4),
        Set(C4, Eb4),
        // Set(C4, Fbb4), // ? 重々減三度
        // Set(Dbb4, Ds4), // ? 重々増一度
        Set(Dbb4, Eb4),
        Set(Dbb4, Fbb4)
      )
    }

    "conbinations(60, 63, 66, 69)" in {

      conbinations(60, 63, 66, 69) shouldEqual Set(
        Set(Bbb4, Gb4, Eb4, Dbb4),
        Set(A4, Fs4, Fbb4, Dbb4),
        Set(Gss4, Fs4, Fbb4, C4),
        Set(A4, Ess4, Eb4, C4),
        Set(Gss4, Ess4, Eb4, Bs3),
        Set(Bbb4, Ess4, Fbb4, C4),
        Set(Gss4, Gb4, Fbb4, Bs3),
        Set(A4, Fs4, Ds4, C4),
        Set(Gss4, Ess4, Ds4, Bs3),
        Set(A4, Gb4, Ds4, C4),
        Set(Bbb4, Gb4, Fbb4, Dbb4),
        Set(A4, Fs4, Fbb4, C4),
        Set(Gss4, Gb4, Ds4, C4),
        Set(A4, Gb4, Eb4, Dbb4),
        Set(Bbb4, Fs4, Fbb4, Bs3),
        Set(Gss4, Ess4, Eb4, C4),
        Set(A4, Ess4, Ds4, Bs3),
        Set(A4, Fs4, Fbb4, Bs3),
        Set(A4, Gb4, Eb4, C4),
        Set(A4, Ess4, Eb4, Dbb4),
        Set(A4, Fs4, Ds4, Bs3),
        Set(A4, Ess4, Fbb4, Dbb4),
        Set(Gss4, Gb4, Fbb4, Dbb4),
        Set(Bbb4, Gb4, Ds4, Bs3),
        Set(A4, Fs4, Ds4, Dbb4),
        Set(Gss4, Gb4, Fbb4, C4),
        Set(Gss4, Gb4, Ds4, Bs3),
        Set(Gss4, Ess4, Fbb4, C4),
        Set(A4, Gb4, Ds4, Dbb4),
        Set(Bbb4, Fs4, Ds4, C4),
        Set(Gss4, Ess4, Ds4, C4),
        Set(Bbb4, Gb4, Ds4, C4),
        Set(A4, Gb4, Ds4, Bs3),
        Set(A4, Ess4, Fbb4, Bs3),
        Set(Gss4, Gb4, Eb4, C4),
        Set(Bbb4, Ess4, Fbb4, Bs3),
        Set(Bbb4, Ess4, Eb4, Bs3),
        Set(Gss4, Ess4, Fbb4, Dbb4),
        Set(Gss4, Gb4, Ds4, Dbb4),
        Set(Bbb4, Ess4, Fbb4, Dbb4),
        Set(A4, Fs4, Eb4, C4),
        Set(Bbb4, Fs4, Eb4, Dbb4),
        Set(Bbb4, Ess4, Eb4, Dbb4),
        Set(A4, Ess4, Ds4, Dbb4),
        Set(Gss4, Fs4, Ds4, Bs3),
        Set(Gss4, Ess4, Eb4, Dbb4),
        Set(Bbb4, Fs4, Eb4, Bs3),
        Set(Gss4, Fs4, Fbb4, Dbb4),
        Set(Bbb4, Ess4, Ds4, Dbb4),
        Set(Bbb4, Ess4, Ds4, Bs3),
        Set(A4, Ess4, Eb4, Bs3),
        Set(Bbb4, Ess4, Ds4, C4),
        Set(Bbb4, Gb4, Eb4, Bs3),
        Set(Bbb4, Fs4, Ds4, Dbb4),
        Set(A4, Ess4, Ds4, C4),
        Set(A4, Gb4, Fbb4, C4),
        Set(A4, Ess4, Fbb4, C4),
        Set(A4, Gb4, Fbb4, Bs3),
        Set(Gss4, Gb4, Eb4, Bs3),
        Set(Gss4, Fs4, Eb4, Bs3),
        Set(Gss4, Ess4, Fbb4, Bs3),
        Set(A4, Fs4, Eb4, Dbb4),
        Set(Bbb4, Gb4, Ds4, Dbb4),
        Set(Bbb4, Fs4, Ds4, Bs3),
        Set(Bbb4, Fs4, Fbb4, C4),
        Set(Gss4, Fs4, Fbb4, Bs3),
        Set(Bbb4, Fs4, Eb4, C4),
        Set(Bbb4, Gb4, Fbb4, Bs3),
        Set(Gss4, Fs4, Eb4, Dbb4),
        Set(A4, Fs4, Eb4, Bs3),
        Set(Bbb4, Ess4, Eb4, C4),
        Set(Bbb4, Fs4, Fbb4, Dbb4),
        Set(Gss4, Ess4, Ds4, Dbb4),
        Set(Bbb4, Gb4, Eb4, C4),
        Set(Gss4, Fs4, Ds4, C4),
        Set(A4, Gb4, Fbb4, Dbb4),
        Set(Gss4, Fs4, Eb4, C4),
        Set(Gss4, Gb4, Eb4, Dbb4),
        Set(Bbb4, Gb4, Fbb4, C4),
        Set(Gss4, Fs4, Ds4, Dbb4),
        Set(A4, Gb4, Eb4, Bs3)
      )
    }

  }

}
