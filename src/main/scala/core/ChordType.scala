package core

sealed trait ChordType {
  def name: String
}

object ChordType {
  object Major extends ChordType {
    override val name = "Major"
  }
  object Minor extends ChordType {
    override val name = "Minor"
  }
  object Seventh extends ChordType {
    override val name = "Seventh"
  }
  object MinorSeventh extends ChordType {
    override val name = "MinorSeventh"
  }
}
