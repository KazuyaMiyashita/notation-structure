package core

case class ChordName(
  chordType: ChordType,
  rootNote: FifthName,
  baseNote: FifthName,
  tensions: Set[Tensions]
) {
  def name: String = {
    val tensionsName = tensions.mkString(" ")
    s"${rootNote.name} ${chordType.name} ${tensionsName} / ${baseNote.name}"
  }
}
