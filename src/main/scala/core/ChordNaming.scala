package core

object ChordNaming {

  val MaxCandicates = 3

  case class ChordPattern(
    chordType: ChordType,
    chordTonesOnC: Set[FifthName],
    avoidNotesOnC: Set[FifthName],
    tensionNotesOnC: Set[FifthName]
  ) {
    def chordTones(root: FifthName): Set[FifthName] = chordTonesOnC.map(_ + root)
    def avoidNotes(root: FifthName): Set[FifthName] = avoidNotesOnC.map(_ + root)
    def tensionNotes(root: FifthName): Set[FifthName] = tensionNotesOnC.map(_ + root)
  }
  val chordPatterns = {
    import ChordType._
    import FifthName._

    ChordPattern(Major, Set(C, E, G), Set(F, Bb), Set(D, Fs, A)) ::
    ChordPattern(Minor, Set(C, Eb, G), Set(Db, Bb), Set(D, G)) ::
    ChordPattern(Seventh, Set(C, E, G, Bb), Set(F), Set(Db, D, Ds, Fs, Ab, A)) ::
    ChordPattern(MinorSeventh, Set(C, Eb, G, Bb), Set(), Set(D, Db, F, Ab, A)) ::
    ChordPattern(MajorSeventh, Set(C, E, G, B), Set(F), Set(D, Fs, A)) :: Nil
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

  case class Candidate(priority: Int, chord: Chord, tensions: Set[Tension])
  def calculateCandidates(pitchs: Set[Pitch]): List[Candidate] = {
    val fifths: Set[FifthName] = pitchs.map(_.fifth)

    for {
      root <- fifths.toList
      chordPattern <- chordPatterns
      chordTones = chordPattern.chordTones(root)
      avoidNotes = chordPattern.avoidNotes(root)
      tensionNotes = chordPattern.tensionNotes(root)
      commonSize = (chordTones & fifths).size if commonSize >= 1
      tensionSize = (tensionNotes & fifths).size
      avoidSize = (avoidNotes & fifths).size
      diffSize = (chordTones &~ fifths).size
      priority = commonSize - avoidSize - diffSize if priority >= 1
    } yield {
      val bass = pitchs.minBy(_.toMidiNoteNumber.value).fifth
      val genten1: Boolean = Set(FifthName.F, FifthName.A, FifthName.Ab).contains(bass - root)
      val genten1num: Int = if (genten1) 1 else 0
      val genten2: Boolean = root != bass
      val genten2num: Int = if (genten2) 1 else 0
      val tensions = ((fifths & tensionNotes) - bass)
        .map(_ - root)
        .flatMap(tensionOnC => tensionPatterns.find(_.pattern == tensionOnC))
        .map(_.tension)
      val chord = Chord(root, chordPattern.chordType, bass, tensions)
      Candidate(2 * priority - 2 * genten1num - genten2num, chord, tensions)
    }

  }

  def calculate(pitchs: Set[Pitch]): Either[Set[Chord], Chord] = {

    if (pitchs.size == 0) Left(Set())
    else {
      val candidates = calculateCandidates(pitchs)
      val maxPriority = candidates.maxBy(_.priority).priority
      val highPriorityCandidates = candidates.filter(_.priority == maxPriority)

      highPriorityCandidates match {
        case Nil => Left(Set())
        case c :: Nil => Right(c.chord)
        case cs if highPriorityCandidates.length <= MaxCandicates => Left(cs.map(_.chord).toSet)
        case _ => Left(Set())
      }
    }

  }

}
