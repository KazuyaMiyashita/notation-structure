package core

object EnharmonicChordNaming {

  val MaxSuitables = 3

  def calculate(pitchss: Set[Set[Pitch]]): Either[Set[Chord], Chord]  = {

    val calculateResults = pitchss.map(ps => ChordNaming.calculate(ps))
    val rights = calculateResults.collect { case Right(v) => v }
    val lefts = calculateResults.collect({ case Left(v) => v }).flatten

    if (rights.isEmpty) Left(lefts)
    else EnharmonicChordNaming.mostSuitable(rights)

  }

  def mostSuitable(chordSet: Set[Chord]): Either[Set[Chord], Chord] = {
    val chords = chordSet.toList

    val chordAvgs: List[(Chord, Double)] = chords.map { chord =>
      val fifths: Set[Fifth] = chord.chordType.pattern.chordTones.map(chord.root + _)
      val avg: Double = fifths.map(_.value).sum.toDouble / fifths.size
      (chord, avg)
    }
    
    val maxPriority = chordAvgs.minBy({ case (chord, avg) =>
      math.abs(avg - 62)
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
