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
  object Diminished extends ChordType {
    override val name = "dim"
  }
  object DiminishedSeventh extends ChordType {
    override val name = "dim7"
  }
  object HalfDiminishedSeventh extends ChordType {
    override val name = "m7-5"
  }
  object Suspended extends ChordType {
    override val name = "sus4"
  }
  object SuspendedSeventh extends ChordType {
    override val name = "7sus4"
  }
  object SeventhFlatFive extends ChordType {
    override val name = "7-5"
  }
  object Augmented extends ChordType {
    override val name = "aug"
  }
  object AugumentedMajorSeventh extends ChordType {
    override val name = "augM7"
  }
  object MinorMajorSeventh extends ChordType {
    override val name = "mM7"
  }

  val all: List[ChordType] = {
    Major ::
    Minor ::
    Seventh ::
    MinorSeventh ::
    MajorSeventh ::
    Diminished ::
    DiminishedSeventh ::
    HalfDiminishedSeventh ::
    Suspended ::
    SuspendedSeventh ::
    SeventhFlatFive ::
    Augmented ::
    AugumentedMajorSeventh ::
    MinorMajorSeventh :: Nil
  }
}
