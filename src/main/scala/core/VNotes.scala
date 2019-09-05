package core

case class VNotes(notes: Set[Note]) extends Set[Note] {
  def iterator: Iterator[Note] = notes.iterator
  def excl(elem: Note): Set[Note] = notes.excl(elem)
  def incl(elem: Note): Set[Note] = notes.incl(elem)
  def contains(elem: Note): Boolean = notes.contains(elem)

  def transpose(that: Note) = VNotes(map(_ + that))
  def calculateChordName: Either[List[Chord], Chord] = ChordNaming.calculate(notes.map(_.pitch))
}

object VNotes {

  def from(ns: Note*): VNotes = new VNotes(ns.toSet)

}
