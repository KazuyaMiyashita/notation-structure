package core

case class Chord(notes: Set[Note], base: Note) {

  def chordName: Either[List[ChordName], ChordName] =
    ChordNaming.judge(notes, base)

}

object Chord {
  def fromVerticalNotes(vnotes: VNotes): Option[Chord] = {
    if (vnotes.size == 0) None
    else if (vnotes.size == 1) Some(Chord(vnotes.notes, vnotes.head))
    else {
      val min = vnotes.minBy(_.toMidiNoteNumber.value)
      Some(Chord(vnotes.notes, min))
    }
  }
}
