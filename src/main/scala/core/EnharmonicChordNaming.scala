package core

object EnharmonicChordNaming {

  val MaxSuitables = 3

  def calculate(pitchss: Set[Set[Pitch]]): Either[Set[Chord], Chord]  = {

    val allCandidates = for {
      pitchs <- pitchss.toList
      candidates <- ChordNaming.calculateCandidates(pitchs)
    } yield (pitchs, candidates)

    if (allCandidates.length == 0) Left(Set())
    else {
      val maxPriority = allCandidates.map(_._2).maxBy(_.scoreing.priority).scoreing.priority
      val highPriorityCandidates = allCandidates.filter(_._2.scoreing.priority == maxPriority)
  
      highPriorityCandidates match {
        case Nil => Left(Set())
        case c :: Nil => Right(c._2.chord)
        case _ => mostSuitable(highPriorityCandidates.map(_._2.chord).toSet)
      }
    }

  }

  def mostSuitable(chordSet: Set[Chord]): Either[Set[Chord], Chord] = {
    val chords = chordSet.toList

    val chordAvgs: List[(Chord, Double)] = chords.map { chord =>
      val fifths: Set[Fifth] = chord.chordType.pattern.chordTones.map(chord.root + _)
      val avg: Double = fifths.map(_.value - FifthName.D.value).sum.toDouble / fifths.size // Dを中心に
      (chord, avg)
    }
    
    val maxPriority = chordAvgs.minBy({ case (chord, avg) =>
      math.abs(avg)
    })(Ordering.Double.TotalOrdering)._2
    val highPriorityChords = chordAvgs.filter(_._2 == maxPriority)
    
    highPriorityChords match {
      case Nil => Left(Set())
      case c :: Nil => Right(c._1)
      case cs if highPriorityChords.length <= MaxSuitables => Left(cs.map(_._1).toSet)
      case _ => Left(Set())
    }

  }


}
