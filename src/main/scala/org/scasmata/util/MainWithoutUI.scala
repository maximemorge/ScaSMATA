// Copyright (C) Maxime MORGE 2018
package org.scasmata.util

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import scala.concurrent.Await
import akka.util.Timeout
import scala.language.postfixOps
import scala.concurrent.duration._

import org.scasmata.environment.Environment
import org.scasmata.actor.{Simulator, Run, Result}

/**
  * Main application without UI
  */
object MainWithoutUI{
  def main(args: Array[String]): Unit = {
    val TIMEOUTVALUE : FiniteDuration = 6000 minutes // Default timeout of a run
    implicit val timeout : Timeout = Timeout(TIMEOUTVALUE)
    val e = new Environment(height = 8, width = 16)
    val system = ActorSystem("ScaSMATASolver") //The Actor system
    val simulator = system.actorOf(Props(classOf[Simulator], e), "Simulator")
    val future = simulator ? Run
    val result = Await.result(future, timeout.duration).isInstanceOf[Result]
    sys.exit(0)
  }
}
