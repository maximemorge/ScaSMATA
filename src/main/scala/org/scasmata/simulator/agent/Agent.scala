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
abstract class Agent(id : Int) extends Actor with FSM[State, Mind]  with Stash with DecisionRule  {
  var simulator: ActorRef = context.parent
  var directory: Directory = new Directory()

  /**
    * Initiates a myopic agent which owns no packet
    */
  startWith(Initial,  new Mind( perception = null, load = None, attempt = None, targets = Nil))

  /**
    * Whatever the state is
    */
  whenUnhandled {
    // If the agent is initiated with the directory
    case Event(Init(d), mind) =>
      this.simulator = sender
      this.directory = d
      if (debug) println(s"Agent$id is ready")
      sender ! Ready
      stay using mind

    // If the agent is killed
    case Event(Kill,mind) =>
      if (debug) println(s"Agent$id is stopped")
      context.stop(self)
      stay using mind

    // In case of unexpected event
    case Event(e, mind) =>
      println(s"Agent$id has received an unexpected event {} in state {}/{}", e, stateName, mind)
      stay using mind

  }
}