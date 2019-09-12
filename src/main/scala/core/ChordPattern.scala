package core

case class ChordPattern(
  chordType: ChordType,
  chordTones: Set[FifthInterval],
  avoidNotes: Set[FifthInterval],
  tensionNotes: Set[Tension]
)

object ChordPattern {

  import ChordType._
  import FifthIntervalName._
  import Tension._

  def of(chordType: ChordType) = chordType match {
    case Major => ChordPattern(Major,Set(PerUnison, MajThird, PerFifth), Set(DimFourth, PerFourth, MinSeventh), Set(Ninth, SharpEleventh, Thirteenth))
    case Minor => ChordPattern(Minor, Set(PerUnison, MinThird, PerFifth), Set(MinSecond, AugSecond, DimThird, DimSixth, MinSeventh, MajSeventh), Set(Ninth, Eleventh))
    case Seventh => ChordPattern(Seventh, Set(PerUnison, MajThird, PerFifth, MinSeventh), Set(AugUnison, DimSecond, MinThird, DimFourth, PerFourth, DimFifth, AugFifth, AugSixth), Set(FlatNinth, Ninth, SharpNinth, SharpEleventh, FlatThirteenth, Thirteenth))
    case MinorSeventh => ChordPattern(MinorSeventh, Set(PerUnison, MinThird, PerFifth, MinSeventh), Set(AugSecond, DimThird, DimSixth), Set(FlatNinth, Ninth, Eleventh, FlatThirteenth, Thirteenth))
    case MajorSeventh => ChordPattern(MajorSeventh, Set(PerUnison, MajThird, PerFifth, MajSeventh), Set(PerFourth, DimFifth, DimOctave), Set(Ninth, SharpEleventh, Thirteenth))
    case Diminished => ChordPattern(Diminished, Set(PerUnison, MinThird, DimFifth), Set(DimSeventh, MinSeventh, MajSeventh, DimOctave), Set())
    case DiminishedSeventh => ChordPattern(DiminishedSeventh, Set(PerUnison, MinThird, DimFifth, DimSeventh), Set(MinSeventh, MajSeventh), Set())
    case HalfDiminishedSeventh => ChordPattern(HalfDiminishedSeventh, Set(PerUnison, MinThird, DimFifth, MinSeventh), Set(DimSeventh), Set())
    case Suspended => ChordPattern(Suspended, Set(PerUnison, PerFourth, PerFifth), Set(MinSecond, MajSecond, MinThird, MajThird, DimSeventh, MinSeventh), Set())
    case SuspendedSeventh => ChordPattern(SuspendedSeventh, Set(PerUnison, PerFourth, PerFifth, MinSeventh), Set(MinSecond, MajSecond, DimThird, MinThird, MajThird, DimFifth, DimSeventh), Set())
    case SeventhFlatFive => ChordPattern(SeventhFlatFive, Set(PerUnison, MajThird, DimFifth, MinSeventh), Set(MinThird, PerFifth, DimSeventh, MajSeventh), Set())
    case Augmented => ChordPattern(Augmented, Set(PerUnison, MajThird, AugFifth), Set(PerFifth, MinSeventh, MajSeventh), Set())
    case AugumentedMajorSeventh => ChordPattern(AugumentedMajorSeventh, Set(PerUnison, MajThird, AugFifth, MajSeventh), Set(PerFifth, MinSeventh), Set())
    case MinorMajorSeventh => ChordPattern(MinorMajorSeventh, Set(PerUnison, MinThird, PerFifth, MajSeventh), Set(MinSeventh), Set(Ninth))
  }

}
