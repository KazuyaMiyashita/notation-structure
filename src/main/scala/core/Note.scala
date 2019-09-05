package core

case class Note(pitch: Pitch, duration: Duration) {

  def +(that: Note) = Note(this.pitch + that.pitch, this.duration + that.duration)
  def -(that: Note) = Note(this.pitch - that.pitch, this.duration - that.duration)

}
