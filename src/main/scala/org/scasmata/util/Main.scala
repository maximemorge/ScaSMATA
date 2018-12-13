// Copyright (C) Maxime MORGE 2018
package org.scasmata.util
import akka.actor.{ActorSystem, Props}
import org.scasmata.environment.Environment
import org.scasmata.actor.{Run, Simulator}

/**
  * Main application
  */
object Main{
  def main(args: Array[String]): Unit = {
    val env = new Environment( height = 4, width = 8, nbAgentBodies = 1, nbPackets = 1, maxSizePackets = 1)
    val ui = new UI(env)
    ui.visible = true

  }
}
