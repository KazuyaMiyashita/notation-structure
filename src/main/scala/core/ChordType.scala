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
  object MajorSeventh extends ChordType {
    override val name = "MajorSeventh"
  }
  object MinorSeventh extends ChordType {
    override val name = "MinorSeventh"
  }
}
