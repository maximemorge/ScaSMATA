// Copyright (C) Maxime MORGE 2018
package org.scasmata.executor

import akka.actor.ActorSystem
import akka.util.Timeout
import scala.concurrent.duration._
import scala.language.postfixOps

import org.scasmata.environment.Environment

/**
  * Abstract class representing an executor
  * @param environment at the beginning
  * @param system of actors
  */
abstract class Executor(val  environment: Environment, val system: ActorSystem) {
  var debug = true

  val TIMEOUTVALUE : FiniteDuration = 6000 minutes // Default timeout of a run
  implicit val timeout : Timeout = Timeout(TIMEOUTVALUE)

  /**
    * Returns the number of steps for each body id to collect all the packets
    */
  def run() : Map[Int,Int]


}
