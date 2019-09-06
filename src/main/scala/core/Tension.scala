package core

sealed trait Tension {
  def name: String
}

object Tension {
  object Ninth extends Tension {
    override def name = "9"
  }
}
