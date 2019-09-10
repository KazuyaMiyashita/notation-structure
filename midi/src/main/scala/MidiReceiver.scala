package midi

import core.MidiNoteNumber
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
    println(notes.map(_.value).mkString(" "))
  }

}
