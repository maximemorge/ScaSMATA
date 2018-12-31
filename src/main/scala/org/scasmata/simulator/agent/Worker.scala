// Copyright (C) Maxime MORGE 2018
package org.scasmata.simulator.agent

import akka.actor.{Actor, ActorRef}
import org.scasmata.environment.{Environment, Packet}
import org.scasmata.simulator._
import org.scasmata.simulator.agent.rule.OperationalRule

/**
  * Perception of the worker
  * @param e its perception of the environment
  * @param load  packet it owns, 0 otherwise
  * @param attempt eventually the last influence emitted
  * @param target eventually the packet to collect
  */
class Perception(val e: Environment, val load: Option[Packet], val attempt: Option[Influence], val target: Option[Packet])

/**
  * Operational agent behaviour
  * @param id of its body
  */
abstract class OperationalAgent(id : Int) extends Actor with OperationalRule {
  var perception = new Perception(e = null, load = None, attempt = None, target = null)
  /**
    * Handle managing
    */
  override def unhandled(message: Any): Unit = message match {
    // If the agent is killed
    case Kill =>
      if (debug) println(s"Worker$id is stopped")
      context.stop(self)
    // In case of unexpected event
    case msg =>
      println(s"Worker$id has received an unexpected event {} with perception {}", msg, perception)

  }
}