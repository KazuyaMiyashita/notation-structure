package core

import org.scalatest._

class NoteNameSpec extends FlatSpec with Matchers {

  it should "convert note name (double flat)" in {

    NoteName(-15).name shouldEqual "Fbb"
    NoteName(-14).name shouldEqual "Cbb"
    NoteName(-13).name shouldEqual "Gbb"
    NoteName(-12).name shouldEqual "Dbb"
    NoteName(-11).name shouldEqual "Abb"
    NoteName(-10).name shouldEqual "Ebb"
    NoteName(-9).name  shouldEqual "Bbb"

  }

  it should "convert note name (flat)" in {

    NoteName(-8).name shouldEqual "Fb"
    NoteName(-7).name shouldEqual "Cb"
    NoteName(-6).name shouldEqual "Gb"
    NoteName(-5).name shouldEqual "Db"
    NoteName(-4).name shouldEqual "Ab"
    NoteName(-3).name shouldEqual "Eb"
    NoteName(-2).name shouldEqual "Bb"

  }

  it should "convert note name (natural)" in {

    NoteName(-1).name shouldEqual "F"
    NoteName(0).name  shouldEqual "C"
    NoteName(1).name  shouldEqual "G"
    NoteName(2).name  shouldEqual "D"
    NoteName(3).name  shouldEqual "A"
    NoteName(4).name  shouldEqual "E"
    NoteName(5).name  shouldEqual "B"

  }

  it should "convert note name (sharp)" in {

    NoteName(6).name  shouldEqual "F#"
    NoteName(7).name  shouldEqual "C#"
    NoteName(8).name  shouldEqual "G#"
    NoteName(9).name  shouldEqual "D#"
    NoteName(10).name shouldEqual "A#"
    NoteName(11).name shouldEqual "E#"
    NoteName(12).name shouldEqual "B#"

  }

  it should "convert note name (double sharp)" in {

    NoteName(13).name shouldEqual "F##"
    NoteName(14).name shouldEqual "C##"
    NoteName(15).name shouldEqual "G##"
    NoteName(16).name shouldEqual "D##"
    NoteName(17).name shouldEqual "A##"
    NoteName(18).name shouldEqual "E##"
    NoteName(19).name shouldEqual "B##"

  }

}
