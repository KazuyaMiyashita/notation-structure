package midi

import core._
import javax.sound.midi.{Receiver, MidiMessage}

class MyMidiReceiver extends Receiver {

  val NoteOn = 144
  val NoteOff = 128

  var notes: Set[MidiNoteNumber] = Set()

  override def close(): Unit = ()

  def send(message: MidiMessage, timeStamp: Long): Unit = {
    val mes: Array[Int] = message.getMessage.map(byte => (byte & 0xFF).toInt)
    val midiNoteNumber = MidiNoteNumber(mes(1))
    if (mes(0) == NoteOn) notes += midiNoteNumber
    else if (mes(0) == NoteOff) notes -= midiNoteNumber

    val chord = chorden(this.notes)
    println(notes.map(_.value).mkString(" ") + " : " + chordToString(chord))
  }

  def chorden(midiNums: Set[MidiNoteNumber]): Either[Set[Chord], Chord] = {
    val enharmonics = midiNums.map(n => Enharmonic(n)).toSet
    val combinate = Enharmonic.combinate(enharmonics)
    EnharmonicChordNaming.calculate(combinate)
  }

  def chordToString(chord: Either[Set[Chord], Chord]): String = chord match {
    case Left(cs) => cs.mkString(" ")
    case Right(c) => c.toString
  }

}
