package core

case class Fifth(value: Int) extends Ordered[Fifth] {

  def compare(that: Fifth): Int = this.value compare that.value

  def +(that: FifthInterval): Fifth = Fifth(this.value + that.value)
  def -(that: FifthInterval): Fifth = Fifth(this.value - that.value)
  def -(that: Fifth): FifthInterval = FifthInterval(this.value - that.value)

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
