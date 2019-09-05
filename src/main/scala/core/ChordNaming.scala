package core

object ChordNaming {

  case class ChordPattern(chordType: ChordType, pattern: Set[FifthName])
  val chordPatterns = {
    import ChordType._
    import FifthName._

    ChordPattern(Major, Set(C, E, G)) ::
    ChordPattern(Minor, Set(C, Eb, G)) ::
    ChordPattern(MajorSeventh, Set(C, E, G, Bb)) ::
    ChordPattern(MinorSeventh, Set(C, Eb, G, Bb)) :: Nil
  }

  def calculate(pitchs: Set[Pitch]): Either[List[ChordName], ChordName] = {

    val base = pitchs.minBy(_.toMidiNoteNumber.value)
    
    def calculateChordTypes: List[(FifthName, ChordType)] = {
      val fifths: Set[FifthName] = pitchs.map(_.fifth)

      val candidates: Seq[(Int, FifthName, ChordType)] = for {
        root <- fifths.toList
        chordPattern <- chordPatterns
        shifted = chordPattern.pattern.map(_ + root)
        common = (shifted & fifths).size if common >= 0
        diff = (shifted &~ fifths).size
      } yield {
        (common - diff, root, chordPattern.chordType)
      }

      val commonMaxNum = candidates.maxBy(_._1)._1
      val commonMax = candidates.filter(_._1 == commonMaxNum)
      commonMax.map(c => (c._2, c._3)).toList
    }

    val chordTypes = calculateChordTypes

    if (pitchs.size == 0) Left(Nil)
    else chordTypes match {
      case Nil => Left(Nil)
      case chordType :: Nil => Right {
        ChordName(chordType._2, chordType._1, base.fifth, Set())
      }
      case _ => Left {
        chordTypes.map { chordType => ChordName(chordType._2, chordType._1, base.fifth, Set()) }
      }
    }

  }

}
