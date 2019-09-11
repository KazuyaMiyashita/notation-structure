package core

object ChordNaming {

  val MaxCandicates = 3

  case class ChordPattern(
    chordType: ChordType,
    chordTones: Set[FifthInterval],
    avoidNotes: Set[FifthInterval],
    tensionNotes: Set[Tension]
  )
  val chordPatterns = {
    import ChordType._
    import FifthIntervalName._
    import Tension._

    ChordPattern(Major,Set(PerUnison, MajThird, PerFifth), Set(PerFourth, MinSeventh), Set(Ninth, SharpEleventh, Thirteenth)) ::
    ChordPattern(Minor, Set(PerUnison, MinThird, PerFifth), Set(MinSecond, MinSeventh), Set(Ninth, Eleventh)) ::
    ChordPattern(Seventh, Set(PerUnison, MajThird, PerFifth, MinSeventh), Set(PerFourth), Set(FlatNinth, Ninth, SharpNinth, SharpEleventh, FlatThirteenth, Thirteenth)) ::
    ChordPattern(MinorSeventh, Set(PerUnison, MinThird, PerFifth, MinSeventh), Set(), Set(FlatNinth, Ninth, Eleventh, FlatThirteenth, Thirteenth)) ::
    ChordPattern(MajorSeventh, Set(PerUnison, MajThird, PerFifth, MajSeventh), Set(PerFourth), Set(Ninth, SharpEleventh, Thirteenth)) ::
    ChordPattern(Diminished, Set(PerUnison, MinThird, DimFifth), Set(DimSeventh, MinSeventh, DimOctave), Set()) ::
    ChordPattern(DiminishedSeventh, Set(PerUnison, MinThird, DimFifth, DimSeventh), Set(MinSeventh), Set()) ::
    ChordPattern(HalfDiminishedSeventh, Set(PerUnison, MinThird, DimFifth, MinSeventh), Set(DimSeventh), Set()) ::
    ChordPattern(Suspended, Set(PerUnison, PerFourth, PerFifth), Set(MinSecond, MajSecond, MinThird, MajThird, DimSeventh, MinSeventh), Set()) ::
    ChordPattern(SuspendedSeventh, Set(PerUnison, PerFourth, PerFifth, MinSeventh), Set(MinSecond, MajSecond, MinThird, MajThird, DimSeventh), Set()) :: Nil
  }

  case class Candidate(scoreing: Scoreing, chord: Chord)
  case class Scoreing(
    intervals: Set[FifthInterval],
    common: Set[FifthInterval],
    avoid: Set[FifthInterval],
    diff1: Set[FifthInterval],
    diff2: Set[FifthInterval],
    genten1: Int,
    genten2: Int
  ) {
    val priority: Int = {
      (4 * common.size) + (-4 * avoid.size) + (-2 * diff1.size) + (-2 * diff2.size) + (-3 * genten1) + (-3 * genten2)
    }
  }
  def calculateCandidates(absPitchs: Set[Pitch]): List[Candidate] = {
    val absFifths: Set[FifthName] = absPitchs.map(_.fifth)
    val absBass: FifthName = absPitchs.minBy(_.toMidiNoteNumber.value).fifth

    for {
      absRoot <- absFifths.toList
      chordPattern <- chordPatterns
    } yield {
      val intervals: Set[FifthInterval] = absPitchs.map(p => p.fifth - absRoot)
      val bass = absBass - absRoot
      val tensions: Set[Tension] = chordPattern.tensionNotes
        .filter(t => intervals(t.interval))
        .filter(t => t.interval != bass)

      val commonChordTones: Set[FifthInterval] = chordPattern.chordTones & intervals
      val commonAvoidNones: Set[FifthInterval] = chordPattern.avoidNotes & intervals

      val diff1: Set[FifthInterval] = chordPattern.chordTones &~ intervals
      val diff2: Set[FifthInterval] = intervals &~ (commonChordTones + bass)

      val genten1: Int = {
        import FifthIntervalName._
        val isGenten = Set(MinSixth, MajSixth).contains(bass)
        if (isGenten) 1 else 0
      }
      val genten2: Int = if (bass != FifthIntervalName.PerUnison) 1 else 0

      val scoreing = Scoreing(
        intervals = intervals,
        common = commonChordTones,
        avoid = commonAvoidNones,
        diff1 = diff1,
        diff2 = diff2,
        genten1, genten2)

      val chord = Chord(absRoot, chordPattern.chordType, absBass, tensions)
      Candidate(scoreing, chord)
    }

  }

  def calculate(pitchs: Set[Pitch]): Either[Set[Chord], Chord] = {

    if (pitchs.size == 0) Left(Set())
    else {
      val candidates = calculateCandidates(pitchs)
      val maxPriority = candidates.maxBy(_.scoreing.priority).scoreing.priority
      val highPriorityCandidates = candidates.filter(_.scoreing.priority == maxPriority)

      highPriorityCandidates match {
        case Nil => Left(Set())
        case c :: Nil => Right(c.chord)
        case cs if highPriorityCandidates.length <= MaxCandicates => Left(cs.map(_.chord).toSet)
        case _ => Left(Set())
      }
    }

  }

}
