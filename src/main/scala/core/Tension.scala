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
}
