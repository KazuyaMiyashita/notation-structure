package core

case class PitchInterval(octave: Int, fifth: FifthInterval) {

  def +(that: PitchInterval) = PitchInterval(this.octave + that.octave, this.fifth + that.fifth)
  def -(that: PitchInterval) = PitchInterval(this.octave - that.octave, this.fifth - that.fifth)

}
