package core

case class HNotes(notes: Seq[Note]) extends Seq[Note] {
  override def apply(i: Int): Note = notes.apply(i)
  override def iterator: Iterator[Note] = notes.iterator
  override def length: Int = notes.length

  def transpose(that: Note) = HNotes(map(_ + that))
}

object HNotes {

  def from(ns: Note*): HNotes = new HNotes(ns)

}
