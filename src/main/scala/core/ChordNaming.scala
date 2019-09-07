package core

object ChordNaming {

  val MaxCandicates = 3

  case class ChordPattern(chordType: ChordType, pattern: Set[FifthName])
  val chordPatterns = {
    import ChordType._
    import FifthName._

    ChordPattern(Major, Set(C, E, G)) ::
    ChordPattern(Minor, Set(C, Eb, G)) ::
    ChordPattern(Seventh, Set(C, E, G, Bb)) ::
    ChordPattern(MinorSeventh, Set(C, Eb, G, Bb)) ::
    ChordPattern(MajorSeventh, Set(C, E, G, B)) :: Nil
  }

  case class TensionPattern(tension: Tension, pattern: FifthName)
  val tensionPatterns = {
    import Tension._
    import FifthName._

    TensionPattern(Ninth, D) ::
    TensionPattern(FlatNinth, Db) ::
    TensionPattern(SharpNinth, Ds) ::
    TensionPattern(Eleventh, F) ::
    TensionPattern(SharpEleventh, Fs) ::
    TensionPattern(Thirteenth, A) ::
    TensionPattern(FlatThirteenth, Ab) :: Nil
  }

  def calculateChords(pitchs: Set[Pitch]): List[Chord] = {
    val fifths: Set[FifthName] = pitchs.map(_.fifth)

    case class Candicate(priority: Int, root: FifthName, chordType: ChordType)
    val candidates: Seq[Candicate] = for {
      root <- fifths.toList
      chordPattern <- chordPatterns
      shifted = chordPattern.pattern.map(_ + root)
      common = (shifted & fifths).size if common >= 1
      diff = (shifted &~ fifths).size
      priority = common - diff if priority >= 1
    } yield {
      Candicate(common - diff, root, chordPattern.chordType)
    }

    println(pitchs.map(p => p.fifth.name).mkString(" "))
    candidates.sortWith((c1, c2) => c1.priority >= c2.priority).foreach { c =>
      val chord = Chord(c.root, c.chordType)
      println(s"${c.priority} : ${chord}")
    }
    println()

    val maxPriority = candidates.maxBy(_.priority).priority
    val highPriorityCandidates = candidates.filter(_.priority == maxPriority)

    if (highPriorityCandidates.length > MaxCandicates) Nil
    else highPriorityCandidates.map(c => Chord(c.root, c.chordType)).toList
  }

  def addBass(chord: Chord, pitchs: Set[Pitch]): Chord = {
    val bass = pitchs.minBy(_.toMidiNoteNumber.value)
    chord.withBass(bass.fifth)
  }

  def addTensions(chord: Chord, pitchs: Set[Pitch]): Chord = {
    val pattern: Set[FifthName] = 
      chordPatterns.find(_.chordType == chord.chordType).get.pattern.map(_ + chord.bass)
    val diff = pitchs.map(_.fifth) &~ pattern

    val tensions: Seq[Tension] = for {
      tensionPattern <- tensionPatterns
      shifted = chord.root + tensionPattern.pattern if diff(shifted)
    } yield tensionPattern.tension

    tensions.foldLeft(chord)((c, t) => c.withTensions(t))
  }

  def calculate(pitchs: Set[Pitch]): Either[Set[Chord], Chord] = {

    val chords = calculateChords(pitchs)
      .map(c => addBass(c, pitchs))
      .map(c => addTensions(c, pitchs))

    if (pitchs.size == 0) Left(Set())
    else chords match {
      case Nil => Left(Set())
      case chord :: Nil => Right(chord)
      case _ => Left(chords.toSet)
    }

  }

}
