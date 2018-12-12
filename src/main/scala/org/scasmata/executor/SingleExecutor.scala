// Copyright (C) Maxime MORGE 2018
package org.scasmata.executor

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask

import scala.concurrent.Await
import scala.language.postfixOps

import org.scasmata.environment.Environment
import org.scasmata.actor.{Start,Outcome,MAScheduler}

/**
  *  An single executor for a single agent with a single packet
  *
  */
class SingleExecutor(override val  environment: Environment, override val system: ActorSystem) extends Executor(environment,system) {

  if (environment.nbAgentBodies != 1 && environment.nbPackets !=1)
    throw new RuntimeException("SingleExecutor runs for a single agent with a single packet")

  // Launch a new MAScheduler
  SingleExecutor.id+=1
  val scheduler : ActorRef = system.actorOf(Props(classOf[MAScheduler], environment), name = "Scheduler"+SingleExecutor.id)

  /**
    * Returns the number of steps for each body to collect all the packets
    */
  def run() : Map[Int,Int] = {
    if (debug) println("Start SingleExecutor")
    val future = scheduler ? Start
    val result = Await.result(future, timeout.duration).asInstanceOf[Outcome]
    if (debug) println("End SingleExecutor")
    result.steps
  }

}

/**
  * Companion object for single executor
  */
object SingleExecutor {
  var id = 0
}