// Copyright (C) Maxime MORGE 2018
package org.scasmata.simulator.agent

import akka.actor.{Actor, ActorRef, FSM, Stash}
import org.scasmata.environment.{Environment, Packet}
import org.scasmata.simulator._
import org.scasmata.simulator.agent.rule.DecisionRule

/**
  * States of the agent
  */
sealed trait State
case object Initial extends State

/**
  * Internal immutable state of mind
  * @param perception its perception
  * @param load  packetId it owns, 0 otherwise
  * @param attempt last influence emitted
  */
class Mind(val perception: Environment, val load: Option[Packet], val attempt: Option[Influence], val targets: Seq[Packet])

/**
  * Agent behaviour
  * @param id of its body
  */
abstract class Agent(id : Int) extends Actor with Stash with DecisionRule  {
  var simulator: ActorRef = context.parent
  var directory: Directory = new Directory()

  var mind = new Mind( perception = null, load = None, attempt = None, targets = Nil)

  /**
    * Whatever the state is
    */
  override def unhandled(message: Any): Unit = message match {
    // If the agent is initiated with the directory
    case Init(d) =>
      this.simulator = sender
      this.directory = d
      if (debug) println(s"Agent$id is ready")
      sender ! Ready

    // If the agent is killed
    case Kill =>
      if (debug) println(s"Agent$id is stopped")
      context.stop(self)

    // In case of unexpected event
    case e =>
      println(s"Agent$id has received an unexpected event {} in state {}", e, mind)

  }
}