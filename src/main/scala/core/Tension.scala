package core

sealed trait Tension {
  def name: String
  def interval: FifthInterval
  override def toString = name
}

object Tension {
  import FifthIntervalName._

  object FlatNinth extends Tension {
    override val name = "b9"
    override val interval = MinSecond + PerOctave
  }
  object Ninth extends Tension {
    override val name = "9"
    override val interval = MajSecond + PerOctave
  }
  object SharpNinth extends Tension {
    override val name = "#9"
    override val interval = AugSecond + PerOctave
  }
  object Eleventh extends Tension {
    override val name = "11"
    override val interval = PerFourth + PerOctave
  }
  object SharpEleventh extends Tension {
    override val name = "#11"
    override val interval = AugFourth + PerOctave
  }
  object FlatThirteenth extends Tension {
    override val name = "b13"
    override val interval = MinSixth + PerOctave
  }
  object Thirteenth extends Tension {
    override val name = "13"
    override val interval = MajSixth + PerOctave
  }

}

object Tensions {
  import Tension._

  def find(absoluteFifth: Fifth, absoluteRoot: Fifth): Option[Tension] = {
    val interval: FifthInterval = absoluteFifth - absoluteRoot
    ts.find(_.interval == interval)
  }

  private val ts =
    Ninth ::
    FlatNinth ::
    SharpNinth ::
    Eleventh ::
    SharpEleventh ::
    Thirteenth ::
    FlatThirteenth :: Nil
}
