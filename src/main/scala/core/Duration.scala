package core

// TODO: Double => Rational
case class Duration(value: Double) {

  def +(that: Duration) = Duration(this.value + that.value)
  def -(that: Duration) = Duration(this.value - that.value)

}
