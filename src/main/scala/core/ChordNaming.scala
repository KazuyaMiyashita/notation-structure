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
      chordPattern <- ChordType.ChordTypes.map(_.pattern)
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

    if (pitchs.size == 0) Left(Set())
    else {
      val candidates = calculateCandidates(pitchs)
      if (candidates.length == 0) Left(Set())
      else {
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

}
