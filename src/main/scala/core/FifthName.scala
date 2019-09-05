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

  val Fbb = FifthName(-15)
  val Cbb = FifthName(-14)
  val Gbb = FifthName(-13)
  val Dbb = FifthName(-12)
  val Abb = FifthName(-11)
  val Ebb = FifthName(-10)
  val Bbb = FifthName(-9)
  val Fb = FifthName(-8)
  val Cb = FifthName(-7)
  val Gb = FifthName(-6)
  val Db = FifthName(-5)
  val Ab = FifthName(-4)
  val Eb = FifthName(-3)
  val Bb = FifthName(-2)
  val F = FifthName(-1)
  val C = FifthName(0)
  val G = FifthName(1)
  val D = FifthName(2)
  val A = FifthName(3)
  val E = FifthName(4)
  val B = FifthName(5)
  val Fs = FifthName(6)
  val Cs = FifthName(7)
  val Gs = FifthName(8)
  val Ds = FifthName(9)
  val As = FifthName(10)
  val Es = FifthName(11)
  val Bs = FifthName(12)
  val Fss = FifthName(13)
  val Css = FifthName(14)
  val Gss = FifthName(15)
  val Dss = FifthName(16)
  val Ass = FifthName(17)
  val Ess = FifthName(18)
  val Bss = FifthName(19)

}
