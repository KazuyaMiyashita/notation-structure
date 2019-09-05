package core

object ChordNaming {

  val MaxCandicates = 3

  case class ChordPattern(chordType: ChordType, pattern: Set[FifthName])
  val chordPatterns = {
    import ChordType._
    import FifthName._

    ChordPattern(Major, Set(C, E, G)) ::
    ChordPattern(Minor, Set(C, Eb, G)) ::
    ChordPattern(MajorSeventh, Set(C, E, G, Bb)) ::
    ChordPattern(MinorSeventh, Set(C, Eb, G, Bb)) :: Nil
  }

  def calculateChords(pitchs: Set[Pitch]): List[Chord] = {
    val fifths: Set[FifthName] = pitchs.map(_.fifth)

    case class Candicate(priority: Int, root: FifthName, chordType: ChordType)
    val candidates: Seq[Candicate] = for {
      root <- fifths.toList
      chordPattern <- chordPatterns
      shifted = chordPattern.pattern.map(_ + root)
      common = (shifted & fifths).size if common >= 0
      diff = (shifted &~ fifths).size
    } yield {
      Candicate(common - diff, root, chordPattern.chordType)
    }

    val maxPriority = candidates.maxBy(_.priority).priority
    val highPriorityCandidates = candidates.filter(_.priority == maxPriority)

    if (highPriorityCandidates.length > MaxCandicates) Nil
    else highPriorityCandidates.map(c => Chord(c.root, c.chordType)).toList
  }

  def addTensions(chord: Chord, pitchs: Set[Pitch]): Chord = {
    val pattern: Set[FifthName] = 
      chordPatterns.find(_.chordType == chord.chordType).get.pattern.map(_ + chord.bass)
    val diff = pitchs.map(_.fifth) &~ pattern
    val shifted = chord.root + FifthName.D

    if (diff(shifted)) chord.withTensions(Tension.AddNinth)
    else chord
  }

  def calculate(pitchs: Set[Pitch]): Either[List[Chord], Chord] = {

    val chords = calculateChords(pitchs).map(c => addTensions(c, pitchs))

    if (pitchs.size == 0) Left(Nil)
    else chords match {
      case Nil => Left(Nil)
      case chord :: Nil => Right(chord)
      case _ => Left(chords)
    }

  }

}
