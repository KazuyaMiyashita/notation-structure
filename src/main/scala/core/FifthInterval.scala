package core

case class FifthInterval(fifth: FifthName) {
  def +(that: FifthInterval) = FifthInterval(this.fifth + that.fifth)
  def -(that: FifthInterval) = FifthInterval(this.fifth - that.fifth)
}
