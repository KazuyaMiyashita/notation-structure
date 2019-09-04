package core

case class NoteSequence(notes: Seq[Note]) extends Seq[Note] {
  override def apply(i: Int): Note = notes.apply(i)
  override def iterator: Iterator[Note] = notes.iterator
  override def length: Int = notes.length

  def transpose(that: Note) = NoteSequence(map(_ + that))
}

object NoteSequence {

  def from(ns: Note*): NoteSequence = new NoteSequence(ns)

}
