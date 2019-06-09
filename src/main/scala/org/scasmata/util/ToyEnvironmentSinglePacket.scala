// Copyright (C) Maxime MORGE 2018
package org.scasmata.util

import akka.actor.{ActorSystem, Props}
import org.scasmata.environment.{Body, Destination, Environment, Packet}
import org.scasmata.gui.UI

/**
  * A toy environment
  */
object ToyEnvironmentSinglePacket {
  val e = new Environment(height = 5, width = 5, n = 1, m= 4, minSizePackets= 1, maxSizePackets = 1)
  e.get(2,2).setContent(Some(new Destination()))
  val body1 = new Body(id = 1)
  val body2 = new Body(id = 2)
  val body3 = new Body(id = 3)
  val body4 = new Body(id = 4)
  e.bodies = Map[Int,Body](1 -> body1, 2-> body2, 3 -> body3, 4-> body4)
  e.get(1,2).setContent(Some(body1))
  e.get(2,3).setContent(Some(body2))
  e.get(3,2).setContent(Some(body3))
  e.get(2,1).setContent(Some(body4))

  val packet1 = new Packet(id = 1, weight = 1)
  val packet2 = new Packet(id = 2, weight = 1)
  val packet3 = new Packet(id = 3, weight = 1)
  val packet4 = new Packet(id = 4, weight = 1)
  e.packets = Map[Int,Packet](1 -> packet1, 2-> packet2, 3 -> packet3, 4-> packet4)
  e.get(0,2).setContent(Some(packet1))
  e.get(2,4).setContent(Some(packet2))
  e.get(4,2).setContent(Some(packet3))
  e.get(2,0).setContent(Some(packet4))

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("ScaSMATASolver") //The Actor system
    system.actorOf(Props(classOf[UI], this.e), "UI") //Run simulator
  }
}
