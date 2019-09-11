package core

sealed trait ChordType {
  def name: String
  def pattern: ChordPattern
}

object ChordType {
  import FifthIntervalName._
  import Tension._

  object Major extends ChordType {
    override val name = ""
    override val pattern = ChordPattern(Major,Set(PerUnison, MajThird, PerFifth), Set(PerFourth, MinSeventh), Set(Ninth, SharpEleventh, Thirteenth))
  }
  object Minor extends ChordType {
    override val name = "m"
    override val pattern = ChordPattern(Minor, Set(PerUnison, MinThird, PerFifth), Set(MinSecond, MinSeventh), Set(Ninth, Eleventh))
  }
  object Seventh extends ChordType {
    override val name = "7"
    override val pattern = ChordPattern(Seventh, Set(PerUnison, MajThird, PerFifth, MinSeventh), Set(PerFourth), Set(FlatNinth, Ninth, SharpNinth, SharpEleventh, FlatThirteenth, Thirteenth))
  }
  object MinorSeventh extends ChordType {
    override val name = "m7"
    override val pattern = ChordPattern(MinorSeventh, Set(PerUnison, MinThird, PerFifth, MinSeventh), Set(), Set(FlatNinth, Ninth, Eleventh, FlatThirteenth, Thirteenth))
  }
  object MajorSeventh extends ChordType {
    override val name = "M7"
    override val pattern = ChordPattern(MajorSeventh, Set(PerUnison, MajThird, PerFifth, MajSeventh), Set(PerFourth), Set(Ninth, SharpEleventh, Thirteenth))
  }
  object Diminished extends ChordType {
    override val name = "dim"
    override val pattern = ChordPattern(Diminished, Set(PerUnison, MinThird, DimFifth), Set(DimSeventh, MinSeventh, DimOctave), Set())
  }
  object DiminishedSeventh extends ChordType {
    override val name = "dim7"
    override val pattern = ChordPattern(DiminishedSeventh, Set(PerUnison, MinThird, DimFifth, DimSeventh), Set(MinSeventh), Set())
  }
  object HalfDiminishedSeventh extends ChordType {
    override val name = "m7-5"
    override val pattern = ChordPattern(HalfDiminishedSeventh, Set(PerUnison, MinThird, DimFifth, MinSeventh), Set(DimSeventh), Set())
  }
  object Suspended extends ChordType {
    override val name = "sus4"
    override val pattern = ChordPattern(Suspended, Set(PerUnison, PerFourth, PerFifth), Set(MinSecond, MajSecond, MinThird, MajThird, DimSeventh, MinSeventh), Set())
  }
  object SuspendedSeventh extends ChordType {
    override val name = "7sus4"
    override val pattern = ChordPattern(SuspendedSeventh, Set(PerUnison, PerFourth, PerFifth, MinSeventh), Set(MinSecond, MajSecond, MinThird, MajThird, DimSeventh), Set())
  }

  val ChordTypes: List[ChordType] = {
    Major ::
    Minor ::
    Seventh ::
    MinorSeventh ::
    MajorSeventh ::
    Diminished ::
    DiminishedSeventh ::
    HalfDiminishedSeventh ::
    Suspended ::
    SuspendedSeventh :: Nil
  }
}
