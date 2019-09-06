package core

sealed trait ChordType {
  def name: String
}

object ChordType {
  object Major extends ChordType {
    override val name = ""
  }
  object Minor extends ChordType {
    override val name = "m"
  }
  object Seventh extends ChordType {
    override val name = "7"
  }
  object MinorSeventh extends ChordType {
    override val name = "m7"
  }
  object MajorSeventh extends ChordType {
    override val name = "M7"
  }
}
