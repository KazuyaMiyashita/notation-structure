package core

case class ChordPattern(
  chordType: ChordType,
  chordTones: Set[FifthInterval],
  avoidNotes: Set[FifthInterval],
  tensionNotes: Set[Tension]
)
