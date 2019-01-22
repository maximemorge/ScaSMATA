// Copyright (C) Maxime MORGE 2018
package org.scasmata.util

import akka.actor.{ActorSystem, Props}
import scala.language.postfixOps
import org.scasmata.environment.Environment


/**
  * Main application
  */
object Main{
  def main(args: Array[String]): Unit = {
    val e = new Environment(height = 8, width = 16, n = 4, m = 8, minSizePackets = 1, maxSizePackets = 2)
    e.init()
    val system = ActorSystem("ScaSMATASolver") //The Actor system
    system.actorOf(Props(classOf[UI], e), "UI")//Run simulator
  }
}
