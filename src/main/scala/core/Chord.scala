package core

case class Chord(
  root: Fifth,
  chordType: ChordType,
  bass: Fifth,
  tensions: Set[Tension]
) {

  def name: String = {
    val tensionsName =
      if (tensions.isEmpty) ""
      else "(" + tensions.map(_.name).mkString(",") + ")"
    val bassName = if (root == bass) "" else s"/${bass.name}"
    s"${root.name}${chordType.name}${tensionsName}${bassName}"
  }

  override def toString = name

  def withBass(b: Fifth): Chord = this.copy(bass = b)
  def withTensions(ts: Tension*): Chord = this.copy(tensions = tensions ++ ts.toSet)
}

object Chord {
  
  def apply(root: Fifth, chordType: ChordType) = {
    new Chord(root, chordType, root, Set())
  }

}
