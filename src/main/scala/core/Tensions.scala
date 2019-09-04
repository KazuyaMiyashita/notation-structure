package core

sealed trait Tensions {
  def name: String
}

object Tensions {
  object AddNinth extends Tensions {
    override def name = "add Ninth"
  }
}
