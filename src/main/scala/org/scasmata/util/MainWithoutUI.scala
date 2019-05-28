// Copyright (C) Maxime MORGE 2018
package org.scasmata.util

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import scala.concurrent.Await
import akka.util.Timeout
import scala.language.postfixOps
import scala.concurrent.duration._

import org.scasmata.environment.Environment
import org.scasmata.simulator.{Simulator, Play, Outcome}

/**
  * Main application without UI
  */
object MainWithoutUI{
  def main(args: Array[String]): Unit = {
    val TIMEOUT_VALUE : FiniteDuration = 6000 minutes // Default timeout of a run
    implicit val timeout : Timeout = Timeout(TIMEOUT_VALUE)
    val conf = new Configuration()
    val e = new Environment(conf.height, conf.width, conf.n, conf.m , conf.maxSizePackets)
    e.init()
    val system = ActorSystem("Solver") //The Actor system
    val simulator = system.actorOf(Props(classOf[Simulator], e, conf.behaviour, conf.rule, 0), "Simulator")
    val future = simulator ? Play
    val result = Await.result(future, timeout.duration).asInstanceOf[Outcome]
    result.steps.foreach{ case (bodyId,step) =>
      println(s"Agent$bodyId has performed $step steps")
    }
    sys.exit(0)
  }
}
