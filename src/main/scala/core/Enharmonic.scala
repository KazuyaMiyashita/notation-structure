package core

case class Enharmonic(midiNoteNumber: MidiNoteNumber) {

  def pitchs: List[Pitch] = {
    val ps = for {
      fifth <- -15 to 19
      n = (midiNoteNumber.value - 60) - (7 * fifth) if n % 12 == 0
      oct = n / 12
    } yield Pitch(oct, Fifth(fifth))
    ps.toList
  }
  def pitchSet: Set[Pitch] = pitchs.toSet

}

object Enharmonic {

  def fromMidiNoteNumber(midiNoteNumber: MidiNoteNumber): Enharmonic = {
    new Enharmonic(midiNoteNumber)
  }

  def combinate(enharmonics: Set[Enharmonic]): Set[Set[Pitch]] = {
    def proc(remaining: List[Enharmonic], acc: List[List[Pitch]])
      :(List[Enharmonic], List[List[Pitch]]) = {

      remaining match {
        case Nil => (Nil, acc)
        case enh :: ts => {
          val next = for {
            ps <- enh.pitchs
            ac <- acc
          } yield (ps :: Nil) ::: ac
          proc(ts, next)
        }
      }
    }

    proc(enharmonics.toList, Nil :: Nil)._2.map(_.toSet).toSet
  }

}
