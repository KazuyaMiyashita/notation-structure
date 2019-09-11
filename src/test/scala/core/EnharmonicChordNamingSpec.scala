package core

import org.scalatest._
import ChordType._
import FifthName._
import Tension._

class EnharmonicChordNamingSpec extends WordSpec with Matchers {

  def nameof(midiNums: Int*): Either[Set[Chord], Chord] = {
    val enharmonics = midiNums.map(n => Enharmonic(MidiNoteNumber(n))).toSet
    val combinate = Enharmonic.combinate(enharmonics)
    EnharmonicChordNaming.calculate(combinate)
  }

  "root C" should {

    "(60, 64, 67) to C" in {
      nameof(60, 64, 67) shouldEqual Right(Chord(C, Major))
    }

  }

}
