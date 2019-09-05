package core

case class Chord(
  root: FifthName,
  chordType: ChordType,
  bass: FifthName,
  tensions: Set[Tensions]
) {

  def name: String = {
    val tensionsName = tensions.mkString(" ")
    s"${root.name} ${chordType.name} ${tensionsName} / ${bass.name}"
  }

  def bass(b: FifthName): Chord = this.copy(bass = b)
  def tensions(ts: Tensions*): Chord = this.copy(tensions = ts.toSet)
}

object Chord {
  
  def apply(root: FifthName, chordType: ChordType) = {
    new Chord(root, chordType, root, Set())
  }

}
