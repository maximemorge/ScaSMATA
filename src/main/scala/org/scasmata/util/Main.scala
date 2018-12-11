// Copyright (C) Maxime MORGE 2018
package org.scasmata.util
import org.scasmata.environment.Environment

/**
  * Main application
  */
object Main{
  def main(args: Array[String]): Unit = {
    val env = new Environment( height = 8, width = 16, maxNumberOfAgents = 4, maxNumberOfPackets = 3)
    val ui = new UI(env)
    ui.visible = true

  }
}
