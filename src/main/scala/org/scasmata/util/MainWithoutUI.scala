// Copyright (C) Maxime MORGE 2018
package org.scasmata.util

import akka.actor.ActorSystem

import org.scasmata.environment.Environment
import org.scasmata.executor.SingleExecutor

/**
  * Main application without UI
  */
object MainWithoutUI{
  def main(args: Array[String]): Unit = {
    val environment = new Environment(height = 8, width = 16)
    val system = ActorSystem("ScaSMATASolver") //The Actor system
    val executor = new SingleExecutor(environment, system)
    executor.run() foreach {case (body, step) => println (body + "-->" + step)}
    sys.exit(0)
  }
}
