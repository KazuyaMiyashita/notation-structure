package core

case class Chord(notes: Set[Note], base: Note)

object Chord {
  def fromVerticalNotes(vnotes: VerticalNotes): Option[Chord] = {
    if (vnotes.size == 0) None
    else if (vnotes.size == 1) Some(Chord(vnotes.notes, vnotes.head))
    else {
      val min = vnotes.minBy(_.toMidiNoteNumber.value)
      Some(Chord(vnotes.notes, min))
    }
  }
}
