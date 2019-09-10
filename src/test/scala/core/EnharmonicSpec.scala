package core

import org.scalatest._
import PitchName._

class EnharmonicSpec extends WordSpec with Matchers {

  def midinameof(midiNum: Int) = Enharmonic(MidiNoteNumber(midiNum)).pitchs

  "def pitchs" should {

    "60 to (Bs3, C4, Dbb4)" in {
      midinameof(60) shouldEqual Set(Bs3, C4, Dbb4)
    }

    "62 to (Css4, D4, Ebb4)" in {
      midinameof(62) shouldEqual Set(Css4, D4, Ebb4)
    }

    "68 to (Gs4, Ab4)" in {
      midinameof(68) shouldEqual Set(Gs4, Ab4)
    }

  }

}
