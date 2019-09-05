package core

import org.scalatest._

class FifthNameSpec extends FlatSpec with Matchers {

  it should "convert note name (double flat)" in {

    FifthName(-15).name shouldEqual "Fbb"
    FifthName(-14).name shouldEqual "Cbb"
    FifthName(-13).name shouldEqual "Gbb"
    FifthName(-12).name shouldEqual "Dbb"
    FifthName(-11).name shouldEqual "Abb"
    FifthName(-10).name shouldEqual "Ebb"
    FifthName(-9).name  shouldEqual "Bbb"

  }

  it should "convert note name (flat)" in {

    FifthName(-8).name shouldEqual "Fb"
    FifthName(-7).name shouldEqual "Cb"
    FifthName(-6).name shouldEqual "Gb"
    FifthName(-5).name shouldEqual "Db"
    FifthName(-4).name shouldEqual "Ab"
    FifthName(-3).name shouldEqual "Eb"
    FifthName(-2).name shouldEqual "Bb"

  }

  it should "convert note name (natural)" in {

    FifthName(-1).name shouldEqual "F"
    FifthName(0).name  shouldEqual "C"
    FifthName(1).name  shouldEqual "G"
    FifthName(2).name  shouldEqual "D"
    FifthName(3).name  shouldEqual "A"
    FifthName(4).name  shouldEqual "E"
    FifthName(5).name  shouldEqual "B"

  }

  it should "convert note name (sharp)" in {

    FifthName(6).name  shouldEqual "F#"
    FifthName(7).name  shouldEqual "C#"
    FifthName(8).name  shouldEqual "G#"
    FifthName(9).name  shouldEqual "D#"
    FifthName(10).name shouldEqual "A#"
    FifthName(11).name shouldEqual "E#"
    FifthName(12).name shouldEqual "B#"

  }

  it should "convert note name (double sharp)" in {

    FifthName(13).name shouldEqual "F##"
    FifthName(14).name shouldEqual "C##"
    FifthName(15).name shouldEqual "G##"
    FifthName(16).name shouldEqual "D##"
    FifthName(17).name shouldEqual "A##"
    FifthName(18).name shouldEqual "E##"
    FifthName(19).name shouldEqual "B##"

  }

}
