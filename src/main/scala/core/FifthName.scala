package core

case class FifthName(value: Int) {

  def +(that: FifthName) = FifthName(this.value + that.value)
  def -(that: FifthName) = FifthName(this.value - that.value)

  def name: String = value match {
    case -15 => "Fbb"
    case -14 => "Cbb"
    case -13 => "Gbb"
    case -12 => "Dbb"
    case -11 => "Abb"
    case -10 => "Ebb"
    case -9  => "Bbb"
    case -8  => "Fb"
    case -7  => "Cb"
    case -6  => "Gb"
    case -5  => "Db"
    case -4  => "Ab"
    case -3  => "Eb"
    case -2  => "Bb"
    case -1  => "F"
    case 0   => "C"
    case 1   => "G"
    case 2   => "D"
    case 3   => "A"
    case 4   => "E"
    case 5   => "B"
    case 6   => "F#"
    case 7   => "C#"
    case 8   => "G#"
    case 9   => "D#"
    case 10  => "A#"
    case 11  => "E#"
    case 12  => "B#"
    case 13  => "F##"
    case 14  => "C##"
    case 15  => "G##"
    case 16  => "D##"
    case 17  => "A##"
    case 18  => "E##"
    case 19  => "B##"
  }

}

object FifthName {

  object Fbb extends FifthName(-15)
  object Cbb extends FifthName(-14)
  object Gbb extends FifthName(-13)
  object Dbb extends FifthName(-12)
  object Abb extends FifthName(-11)
  object Ebb extends FifthName(-10)
  object Bbb extends FifthName(-9)
  object Fb extends FifthName(-8)
  object Cb extends FifthName(-7)
  object Gb extends FifthName(-6)
  object Db extends FifthName(-5)
  object Ab extends FifthName(-4)
  object Eb extends FifthName(-3)
  object Bb extends FifthName(-2)
  object F extends FifthName(-1)
  object C extends FifthName(0)
  object G extends FifthName(1)
  object D extends FifthName(2)
  object A extends FifthName(3)
  object E extends FifthName(4)
  object B extends FifthName(5)
  object Fs extends FifthName(6)
  object Cs extends FifthName(7)
  object Gs extends FifthName(8)
  object Ds extends FifthName(9)
  object As extends FifthName(10)
  object Es extends FifthName(11)
  object Bs extends FifthName(12)
  object Fss extends FifthName(13)
  object Css extends FifthName(14)
  object Gss extends FifthName(15)
  object Dss extends FifthName(16)
  object Ass extends FifthName(17)
  object Ess extends FifthName(18)
  object Bss extends FifthName(19)

}
