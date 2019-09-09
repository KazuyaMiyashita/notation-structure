package core

sealed trait Tension {
  def name: String
  def interval: Interval
  override def toString = name
}

object Tension {
  import Interval._

  object Ninth extends Tension {
    override val name = "9"
    override val interval = MajorSecond + PerfectOctave
  }
  object FlatNinth extends Tension {
    override val name = "b9"
    override val interval = MinorSecond + PerfectOctave
  }
  object SharpNinth extends Tension {
    override val name = "#9"
    override val interval = AugumentedSecond + PerfectOctave
  }
  object Eleventh extends Tension {
    override val name = "11"
    override val interval = PerfectFourth + PerfectOctave
  }
  object SharpEleventh extends Tension {
    override val name = "#11"
    override val interval = AugumentedFourth + PerfectOctave
  }
  object Thirteenth extends Tension {
    override val name = "13"
    override val interval = MajorSixth + PerfectOctave
  }
  object FlatThirteenth extends Tension {
    override val name = "b13"
    override val interval = MinorSixth + PerfectOctave
  }

}

object Tensions {
  import Tension._

  def find(fifthName: FifthName, root: FifthName): Option[Tension] = {
    val interval = fifthName - root
    ts.find(_.interval.pitchOnC.fifth == interval)
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
