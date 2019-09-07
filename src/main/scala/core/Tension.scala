package core

sealed trait Tension {
  def name: String
}

object Tension {
  object Ninth extends Tension {
    override def name = "9"
  }
  object FlatNinth extends Tension {
    override def name = "b9"
  }
  object SharpNinth extends Tension {
    override def name = "#9"
  }
  object Eleventh extends Tension {
    override def name = "11"
  }
  object SharpEleventh extends Tension {
    override def name = "#11"
  }
  object Thirteenth extends Tension {
    override def name = "13"
  }
  object FlatThirteenth extends Tension {
    override def name = "b13"
  }
}
