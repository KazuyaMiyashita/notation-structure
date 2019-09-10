package midi

import javax.sound.midi._
import scala.util.{Try, Success, Failure}

object Main extends App {
  println("hello midi")
  
  val infos = MidiSystem.getMidiDeviceInfo().toList
  val devices: Try[List[MidiDevice]] = Try {
    infos.map { info => MidiSystem.getMidiDevice(info) }
  }

  def dumpDeviceInfo(device: MidiDevice, num: Int): Unit = {
    val info: MidiDevice.Info = device.getDeviceInfo();
    println(s"[${num}] devinfo: " + info.toString())
    println("  name:"        + info.getName())
    println("  vendor:"      + info.getVendor())
    println("  version:"     + info.getVersion())
    println("  description:" + info.getDescription())
    if (device.isInstanceOf[Synthesizer]) {
      println("  SYNTHESIZER")
    }
    if (device.isInstanceOf[Sequencer]) {
      println("  SEQUENCER")
    }
    println("");
  }

  devices match {
    case Failure(e: SecurityException) => println(e)
    case Failure(e: MidiUnavailableException) => println(e)
    case Failure(e) => println(e)
    case Success(devices) => {
      if (devices.length == 0) println("no devices")
      else {
        devices.zipWithIndex.foreach { case (device, num) => dumpDeviceInfo(device, num) }
        println("enter your midi device number >")
        val deviceNum = io.StdIn.readLine().toInt
        println(deviceNum)
        val device = devices(deviceNum)
        val receiver = new MyMidiReceiver
        device.open()
        device.getTransmitter().setReceiver(receiver)
        println("enter to exit >")
        io.StdIn.readLine()
        device.close()
        receiver.close()
      }
    }
  }

}
