package core

object ChordNaming {

  val MaxCandicates = 3

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
    val absFifths: Set[Fifth] = absPitchs.map(_.fifth)
    val absBass: Fifth = absPitchs.minBy(_.toMidiNoteNumber.value).fifth

    for {
      absRoot <- absFifths.toList
      chordPattern <- ChordType.all.map(ChordPattern.of(_))
      intervals: Set[FifthInterval] = absPitchs.map(p => p.fifth - absRoot)
      commonChordTones: Set[FifthInterval] = chordPattern.chordTones & intervals if commonChordTones.size >= 3
    } yield {
      val bass = absBass - absRoot
      val tensions: Set[Tension] = chordPattern.tensionNotes
        .filter(t => intervals(t.interval))
        .filter(t => t.interval != bass)
      
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

    def checkSize[A <: Iterable[_]](elems: A): Either[Set[Chord], A] =
      if (elems.size == 0) Left(Set()) else Right(elems)

    def filterResult(highPriorityCandidates: List[Candidate]): Either[Set[Chord], Chord] = {
      highPriorityCandidates match {
        case Nil => Left(Set())
        case c :: Nil => Right(c.chord)
        case cs if highPriorityCandidates.length <= MaxCandicates => Left(cs.map(_.chord).toSet)
        case _ => Left(Set())
      }
    }

    for {
      nonEmptyPitchs <- checkSize(pitchs)
      candidates = calculateCandidates(nonEmptyPitchs)
      nonEmptyCandidates <- checkSize(candidates)
      maxPriority = nonEmptyCandidates.maxBy(_.scoreing.priority).scoreing.priority
      highPriorityCandidates = candidates.filter(_.scoreing.priority == maxPriority)
      result <- filterResult(highPriorityCandidates)
    } yield result

  }

}
