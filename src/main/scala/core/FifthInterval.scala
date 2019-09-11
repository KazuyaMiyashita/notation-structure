package core

case class FifthInterval(value: Int) {
  
  def +(that: FifthInterval) = FifthInterval(this.value + that.value)
  def -(that: FifthInterval) = FifthInterval(this.value - that.value)

  def name = {
    import FifthIntervalName._
    this match {
      case PerUnison => "P1"
      case AugUnison => "A1"
      case DimSecond => "d2"
      case MinSecond => "m2"
      case MajSecond => "M2"
      case AugSecond => "A2"
      case DimThird => "d3"
      case MinThird => "m3"
      case MajThird => "M3"
      case AugThird => "A3"
      case DimFourth => "d4"
      case PerFourth => "P4"
      case AugFourth => "A4"
      case DimFifth => "d5"
      case PerFifth => "P5"
      case AugFifth => "A5"
      case DimSixth => "d6"
      case MinSixth => "m6"
      case MajSixth => "M6"
      case AugSixth => "A6"
      case DimSeventh => "d7"
      case MinSeventh => "m7"
      case MajSeventh => "M7"
      case AugSeventh => "A7"
      case DimOctave => "d8"
      case PerOctave => "P8"
      case AugOctave => "A8"
      case _ => s"FifthInterval(${value})"
    }
  }
  override def toString = name
}
