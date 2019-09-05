package midi

import javax.sound.midi._
import scala.util.{Try, Success, Failure}

object Main extends App {
  println("hello midi")
  
  MidiSystem.getMidiDeviceInfo().foreach(println)
  
  val infos = MidiSystem.getMidiDeviceInfo().toList
  val devices: Try[List[MidiDevice]] = Try {
    infos.map { info => MidiSystem.getMidiDevice(info) }
  }

  devices match {
    case Failure(e: SecurityException) => println(e)
    case Failure(e: MidiUnavailableException) => println(e)
    case Failure(e) => println(e)
    case Success(devices) => {
      if (devices.length == 0) println("no devices")
      else devices.foreach(println)
    }
  }

}
