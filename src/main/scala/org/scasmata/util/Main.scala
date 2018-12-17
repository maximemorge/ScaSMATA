// Copyright (C) Maxime MORGE 2018
package org.scasmata.util

import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import scala.language.postfixOps
import scala.concurrent.duration._
import scala.concurrent.duration.FiniteDuration

import org.scasmata.actor.Simulator
import org.scasmata.environment.Environment


/**
  * Main application
  */
object Main{
  def main(args: Array[String]): Unit = {
    val TIMEOUTVALUE : FiniteDuration = 6000 minutes // Default timeout of a run
    implicit val timeout : Timeout = Timeout(TIMEOUTVALUE)
    val e = new Environment(height = 8, width = 8, nbAgentBodies = 2, nbPackets = 4)
    val ui = new UI(e)
    ui.visible = true
  }
}
