package core

case class Enharmonic(midiNoteNumber: MidiNoteNumber) {

  def pitchs: Set[Pitch] = {
    val ps = for {
      fifth <- -15 to 19
      n = (midiNoteNumber.value - 60) - (7 * fifth)
      oct = n / 12 if n % 12 == 0
    } yield Pitch(oct, FifthName(fifth))
    ps.toSet
  }

}

object Enharmonic {

  def fromMidiNoteNumber(midiNoteNumber: MidiNoteNumber): Enharmonic = {
    new Enharmonic(midiNoteNumber)
  }

  def combinate(enharmonics: Seq[Enharmonic]): Seq[Set[Pitch]] = {
    ???
  }

}
