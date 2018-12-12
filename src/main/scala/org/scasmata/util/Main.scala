// Copyright (C) Maxime MORGE 2018
package org.scasmata.util
import akka.actor.ActorSystem
import org.scasmata.environment.Environment
import org.scasmata.executor.SingleExecutor

/**
  * Main application
  */
object Main{
  def main(args: Array[String]): Unit = {
    val env = new Environment( height = 4, width = 8, nbAgentBodies = 1, nbPackets = 1, maxSizePackets = 1)
    val system = ActorSystem("MScaSMATASolver") //The Actor system
    val executor = new SingleExecutor(env, system)
    val ui = new UI(env,executor)
    ui.visible = true

  }
}
