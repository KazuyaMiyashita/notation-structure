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
    ChordPattern(MajorSeventh, Set(PerUnison, MajThird, PerFifth, MajSeventh), Set(PerFourth), Set(Ninth, SharpEleventh, Thirteenth)) :: Nil
  }

  case class Candidate(scoreing: Scoreing, chord: Chord)
  case class Scoreing(commonSize: Int, avoidSize: Int, diffSize: Int, genten1: Int, genten2: Int) {
    val priority: Int = {
      2 * (commonSize - avoidSize - diffSize) - 2 * genten1 - genten2
    }
  }
  def calculateCandidates(absPitchs: Set[Pitch]): List[Candidate] = {
    val absFifths: Set[FifthName] = absPitchs.map(_.fifth)
    val absBass: FifthName = absPitchs.minBy(_.toMidiNoteNumber.value).fifth

    // for {
    //   root <- absoluteFifths.toList
    //   chordPattern <- chordPatterns
    //   // chordTones = chordPattern.chordTones(root)
    //   // avoidNotes = chordPattern.avoidNotes(root)
    //   // tensionNotes = chordPattern.tensionNotes(root)
    //   // commonSize = (chordTones & absoluteFifths).size if commonSize >= 1
    //   // tensionSize = (tensionNotes & absoluteFifths).size
    //   // avoidSize = (avoidNotes & absoluteFifths).size
    //   // diffSize = (chordTones &~ absoluteFifths).size
    //   // priority = commonSize - avoidSize - diffSize if priority >= 1
    // } yield {
    //   // val bass = absolutePitchs.minBy(_.toMidiNoteNumber.value).fifth
    //   // val genten1: Boolean = Set(FifthName.F, FifthName.A, FifthName.Ab).contains(bass - root)
    //   // val genten1num: Int = if (genten1) 1 else 0
    //   // val genten2: Boolean = root != bass
    //   // val genten2num: Int = if (genten2) 1 else 0
    //   // val tensions = ((absoluteFifths & tensionNotes) - bass).flatMap(f => Tensions.find(f, root))
    //   // val chord = Chord(root, chordPattern.chordType, bass, tensions)
    //   // Candidate(2 * priority - 2 * genten1num - genten2num, chord, tensions)
    //   ???
    // }

    for {
      absRoot <- absFifths.toList
      chordPattern <- chordPatterns
    } yield {
      val intervals: Set[FifthInterval] = absPitchs.map(p => FifthInterval(p.fifth - absRoot))
      val commonChordTones: Set[FifthInterval] = chordPattern.chordTones & intervals
      val commonAvoidNones: Set[FifthInterval] = chordPattern.avoidNotes & intervals
      val commonTensionNotes: Set[Tension] = chordPattern.tensionNotes.filter(t => intervals(t.interval))

      val bass = FifthInterval(absBass - absRoot)

      val genten1: Int = {
        import FifthIntervalName._
        val isGenten = Set(PerFourth, MinSixth, MajSixth).contains(bass)
        if (isGenten) 1 else 0
      }
      val genten2: Int = if (bass != FifthIntervalName.PerUnison) 1 else 0

      val scoreing = Scoreing(
        commonSize = commonChordTones.size,
        avoidSize = commonAvoidNones.size,
        diffSize = (chordPattern.chordTones &~ intervals).size,
        genten1, genten2)

      val chord = Chord(absRoot, chordPattern.chordType, absBass, commonTensionNotes)
      Candidate(scoreing, chord)
    }

  }

  def calculate(pitchs: Set[Pitch]): Either[Set[Chord], Chord] = {

    if (pitchs.size == 0) Left(Set())
    else {
      val candidates = calculateCandidates(pitchs)
      val maxPriority = candidates.maxBy(_.scoreing.priority).scoreing.priority
      val highPriorityCandidates = candidates.filter(_.scoreing.priority == maxPriority)

      println(pitchs.map(_.fifth.name).mkString(" "))
      candidates.sortWith((c1, c2) => c1.scoreing.priority >= c2.scoreing.priority).foreach { c =>
        print(c.scoreing.priority.toString + " : ")
        println(c)
      }
      println()

      highPriorityCandidates match {
        case Nil => Left(Set())
        case c :: Nil => Right(c.chord)
        case cs if highPriorityCandidates.length <= MaxCandicates => Left(cs.map(_.chord).toSet)
        case _ => Left(Set())
      }
    }

  }

}
