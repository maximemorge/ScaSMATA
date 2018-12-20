// Copyright (C) Maxime MORGE 2018
package org.scasmata.util

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import scala.concurrent.Await
import akka.util.Timeout
import scala.language.postfixOps
import scala.concurrent.duration._

import org.scasmata.environment.Environment
import org.scasmata.actor.{Simulator, Play, Outcome}

/**
  * Main application without UI
  */
object MainWithoutUI{
  def main(args: Array[String]): Unit = {
    val TIMEOUTVALUE : FiniteDuration = 6000 minutes // Default timeout of a run
    implicit val timeout : Timeout = Timeout(TIMEOUTVALUE)
    val e = new Environment(height = 4, width = 8, minSizePackets = 1)
    val system = ActorSystem("ScaSMATASolver") //The Actor system
    val simulator = system.actorOf(Props(classOf[Simulator], e, 0), "Simulator")
    val future = simulator ? Play
    val result = Await.result(future, timeout.duration).asInstanceOf[Outcome]
    result.steps.foreach{ case (bodyId,step) =>
      println(s"Agent$bodyId has performed $step steps")
    }
    sys.exit(0)
  }
}
