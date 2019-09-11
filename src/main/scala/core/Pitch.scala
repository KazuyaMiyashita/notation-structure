package core

case class Pitch(octave: Int, fifth: FifthName) {

  def +(that: PitchInterval) = Pitch(this.octave + that.octave, this.fifth + that.fifth)
  def -(that: PitchInterval) = Pitch(this.octave - that.octave, this.fifth - that.fifth)

  def toMidiNoteNumber: MidiNoteNumber = {
    MidiNoteNumber(60 + (octave * 12) + (fifth.value * 7))
  }

  def name = {
    import PitchName._
    this match {
      case Cbb1 => "Cbb1"
      case Dbb1 => "Dbb1"
      case Ebb1 => "Ebb1"
      case Fbb1 => "Fbb1"
      case Gbb1 => "Gbb1"
      case Abb1 => "Abb1"
      case Bbb1 => "Bbb1"
      case Cbb2 => "Cbb2"
      case Dbb2 => "Dbb2"
      case Ebb2 => "Ebb2"
      case Fbb2 => "Fbb2"
      case Gbb2 => "Gbb2"
      case Abb2 => "Abb2"
      case Bbb2 => "Bbb2"
      case Cbb3 => "Cbb3"
      case Dbb3 => "Dbb3"
      case Ebb3 => "Ebb3"
      case Fbb3 => "Fbb3"
      case Gbb3 => "Gbb3"
      case Abb3 => "Abb3"
      case Bbb3 => "Bbb3"
      case Cbb4 => "Cbb4"
      case Dbb4 => "Dbb4"
      case Ebb4 => "Ebb4"
      case Fbb4 => "Fbb4"
      case Gbb4 => "Gbb4"
      case Abb4 => "Abb4"
      case Bbb4 => "Bbb4"
      case Cbb5 => "Cbb5"
      case Dbb5 => "Dbb5"
      case Ebb5 => "Ebb5"
      case Fbb5 => "Fbb5"
      case Gbb5 => "Gbb5"
      case Abb5 => "Abb5"
      case Bbb5 => "Bbb5"
      case Cbb6 => "Cbb6"
      case Dbb6 => "Dbb6"
      case Ebb6 => "Ebb6"
      case Fbb6 => "Fbb6"
      case Gbb6 => "Gbb6"
      case Abb6 => "Abb6"
      case Bbb6 => "Bbb6"
      case Cbb7 => "Cbb7"
      case Cb1 => "Cb1"
      case Db1 => "Db1"
      case Eb1 => "Eb1"
      case Fb1 => "Fb1"
      case Gb1 => "Gb1"
      case Ab1 => "Ab1"
      case Bb1 => "Bb1"
      case Cb2 => "Cb2"
      case Db2 => "Db2"
      case Eb2 => "Eb2"
      case Fb2 => "Fb2"
      case Gb2 => "Gb2"
      case Ab2 => "Ab2"
      case Bb2 => "Bb2"
      case Cb3 => "Cb3"
      case Db3 => "Db3"
      case Eb3 => "Eb3"
      case Fb3 => "Fb3"
      case Gb3 => "Gb3"
      case Ab3 => "Ab3"
      case Bb3 => "Bb3"
      case Cb4 => "Cb4"
      case Db4 => "Db4"
      case Eb4 => "Eb4"
      case Fb4 => "Fb4"
      case Gb4 => "Gb4"
      case Ab4 => "Ab4"
      case Bb4 => "Bb4"
      case Cb5 => "Cb5"
      case Db5 => "Db5"
      case Eb5 => "Eb5"
      case Fb5 => "Fb5"
      case Gb5 => "Gb5"
      case Ab5 => "Ab5"
      case Bb5 => "Bb5"
      case Cb6 => "Cb6"
      case Db6 => "Db6"
      case Eb6 => "Eb6"
      case Fb6 => "Fb6"
      case Gb6 => "Gb6"
      case Ab6 => "Ab6"
      case Bb6 => "Bb6"
      case Cb7 => "Cb7"
      case C1 => "C1"
      case D1 => "D1"
      case E1 => "E1"
      case F1 => "F1"
      case G1 => "G1"
      case A1 => "A1"
      case B1 => "B1"
      case C2 => "C2"
      case D2 => "D2"
      case E2 => "E2"
      case F2 => "F2"
      case G2 => "G2"
      case A2 => "A2"
      case B2 => "B2"
      case C3 => "C3"
      case D3 => "D3"
      case E3 => "E3"
      case F3 => "F3"
      case G3 => "G3"
      case A3 => "A3"
      case B3 => "B3"
      case C4 => "C4"
      case D4 => "D4"
      case E4 => "E4"
      case F4 => "F4"
      case G4 => "G4"
      case A4 => "A4"
      case B4 => "B4"
      case C5 => "C5"
      case D5 => "D5"
      case E5 => "E5"
      case F5 => "F5"
      case G5 => "G5"
      case A5 => "A5"
      case B5 => "B5"
      case C6 => "C6"
      case D6 => "D6"
      case E6 => "E6"
      case F6 => "F6"
      case G6 => "G6"
      case A6 => "A6"
      case B6 => "B6"
      case C7 => "C7"
      case Cs1 => "Cs1"
      case Ds1 => "Ds1"
      case Es1 => "Es1"
      case Fs1 => "Fs1"
      case Gs1 => "Gs1"
      case As1 => "As1"
      case Bs1 => "Bs1"
      case Cs2 => "Cs2"
      case Ds2 => "Ds2"
      case Es2 => "Es2"
      case Fs2 => "Fs2"
      case Gs2 => "Gs2"
      case As2 => "As2"
      case Bs2 => "Bs2"
      case Cs3 => "Cs3"
      case Ds3 => "Ds3"
      case Es3 => "Es3"
      case Fs3 => "Fs3"
      case Gs3 => "Gs3"
      case As3 => "As3"
      case Bs3 => "Bs3"
      case Cs4 => "Cs4"
      case Ds4 => "Ds4"
      case Es4 => "Es4"
      case Fs4 => "Fs4"
      case Gs4 => "Gs4"
      case As4 => "As4"
      case Bs4 => "Bs4"
      case Cs5 => "Cs5"
      case Ds5 => "Ds5"
      case Es5 => "Es5"
      case Fs5 => "Fs5"
      case Gs5 => "Gs5"
      case As5 => "As5"
      case Bs5 => "Bs5"
      case Cs6 => "Cs6"
      case Ds6 => "Ds6"
      case Es6 => "Es6"
      case Fs6 => "Fs6"
      case Gs6 => "Gs6"
      case As6 => "As6"
      case Bs6 => "Bs6"
      case Cs7 => "Cs7"
      case Css1 => "Css1"
      case Dss1 => "Dss1"
      case Ess1 => "Ess1"
      case Fss1 => "Fss1"
      case Gss1 => "Gss1"
      case Ass1 => "Ass1"
      case Bss1 => "Bss1"
      case Css2 => "Css2"
      case Dss2 => "Dss2"
      case Ess2 => "Ess2"
      case Fss2 => "Fss2"
      case Gss2 => "Gss2"
      case Ass2 => "Ass2"
      case Bss2 => "Bss2"
      case Css3 => "Css3"
      case Dss3 => "Dss3"
      case Ess3 => "Ess3"
      case Fss3 => "Fss3"
      case Gss3 => "Gss3"
      case Ass3 => "Ass3"
      case Bss3 => "Bss3"
      case Css4 => "Css4"
      case Dss4 => "Dss4"
      case Ess4 => "Ess4"
      case Fss4 => "Fss4"
      case Gss4 => "Gss4"
      case Ass4 => "Ass4"
      case Bss4 => "Bss4"
      case Css5 => "Css5"
      case Dss5 => "Dss5"
      case Ess5 => "Ess5"
      case Fss5 => "Fss5"
      case Gss5 => "Gss5"
      case Ass5 => "Ass5"
      case Bss5 => "Bss5"
      case Css6 => "Css6"
      case Dss6 => "Dss6"
      case Ess6 => "Ess6"
      case Fss6 => "Fss6"
      case Gss6 => "Gss6"
      case Ass6 => "Ass6"
      case Bss6 => "Bss6"
      case Css7 => "Css7"
      case _ => s"Pitch(${octave}, ${fifth.value})"
    }
  }
  override def toString = name

}

object Pitch {
  def apply(octave: Int, fifthValue: Int) = new Pitch(octave, FifthName(fifthValue))
}
