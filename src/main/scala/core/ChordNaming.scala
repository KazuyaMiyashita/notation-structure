package core

object ChordNaming {

  def calculate(notes: Set[Pitch]): Either[List[ChordName], ChordName] = {

    val base = notes.minBy(_.toMidiNoteNumber.value)
    
    type RootFifth = Int
    def calculateChordTypes: List[(RootFifth, ChordType)] = {
      val fifths: Set[RootFifth] = notes.map(_.fifth)

      val candidates: Seq[(Int, RootFifth, ChordType)] = for {
        root <- fifths.min to fifths.max
        chordType <- ChordType.ChordTypes
        shifted = chordType.pattern.map(_ + root)
        common = (shifted & fifths).size if common >= 0
        diff = (shifted &~ fifths).size
      } yield {
        (common - diff, root, chordType)
      }

      val commonMaxNum = candidates.maxBy(_._1)._1
      val commonMax = candidates.filter(_._1 == commonMaxNum)
      commonMax.map(c => (c._2, c._3)).toList
    }

    val chordTypes = calculateChordTypes

    if (notes.size == 0) Left(Nil)
    else chordTypes match {
      case Nil => Left(Nil)
      case chordType :: Nil => Right {
        ChordName(chordType._2, NoteName(chordType._1), NoteName(base.fifth), Set())
      }
      case _ => Left {
        chordTypes.map { chordType => ChordName(chordType._2, NoteName(chordType._1), NoteName(base.fifth), Set()) }
      }
    }

  }

}
