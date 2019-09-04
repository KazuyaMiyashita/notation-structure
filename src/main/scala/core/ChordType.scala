package core

sealed trait ChordType {
  def name: String
  private[core] def pattern: Set[Int]
}

object ChordType {
  object Major extends ChordType {
    override val name = "Major"
    override val pattern = Set(0, 4, 1) // C, E, G
  }
  object Minor extends ChordType {
    override val name = "Minor"
    override val pattern = Set(0, -3, 1) // C, Eb, G
  }
  object MajorSeventh extends ChordType {
    override val name = "MajorSeventh"
    override val pattern = Set(0, 4, 1, -2) // C, E, G, Bb
  }
  object MinorSeventh extends ChordType {
    override val name = "MinorSeventh"
    override val pattern = Set(0, -3, 1, -2) // C, Eb, G, Bb
  }

  val ChordTypes = Major :: Minor :: MajorSeventh :: MinorSeventh :: Nil
}
