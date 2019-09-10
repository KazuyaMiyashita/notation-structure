package core

case class Chord(
  root: FifthName,
  chordType: ChordType,
  bass: FifthName,
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

  def withBass(b: FifthName): Chord = this.copy(bass = b)
  def withTensions(ts: Tension*): Chord = this.copy(tensions = tensions ++ ts.toSet)
}

object Chord {
  
  def apply(root: FifthName, chordType: ChordType) = {
    new Chord(root, chordType, root, Set())
  }

}
