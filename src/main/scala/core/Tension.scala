package core

sealed trait Tension {
  def name: String
}

object Tension {
  object AddNinth extends Tension {
    override def name = "add Ninth"
  }
}
