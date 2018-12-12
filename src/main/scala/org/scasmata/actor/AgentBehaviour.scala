// Copyright (C) Maxime MORGE 2018
package org.scasmata.actor

import akka.actor.{Actor, ActorRef, FSM, Stash}

import scala.language.postfixOps
import org.scasmata.environment.Environment

/**
  * States of the agent
  */
sealed trait State
case object Initial extends State

/**
  * TODO Internal immutable state of mind
  */
class Perception(height : Int, width : Int) extends Environment(height, width)

/**
  * Agent behaviour
   * @param id of the body
  */
class AgentBehaviour(id : Int) extends Actor with FSM[State, Environment] with Stash {
  val debug = true

  var scheduler: ActorRef = context.parent
  var directory: Directory = new Directory()

  /**
    * Initiates a myopic agent
    */
  startWith(Initial, null)


  /**
    * Either the agent is in the initial state
    */
  when(Initial) {
    // If the perception is updated
    case Event(Update(env), mind) =>
      if (debug) println(s"Agent$id : it is updated about the environment")
      // TODO Decide
      if (debug) println(s"Agent$id : it decides to stop")
      sender ! Finished(0)
      stay using env
  }

  /**
    * Whatever the state is
    */
  whenUnhandled {
    // If the agent is initiated with the directory
    case Event(Init(directory), mind) =>
      this.scheduler = sender
      if (debug) println(s"Agent$id : it is ready")
      sender ! Ready
      stay using mind

    // If the agent is stopped
    case Event(Stop,mind) =>
      if (debug) println(s"Agent$id : it is stopped")
      context.stop(self)
      stay using mind

    // In case of unexpected event
    case Event(e, mind) =>
      println(s"${id}: ERROR  unexpected event {} in state {}/{}", e, stateName, mind)
      stay using mind

  }

  // Finally Triggering it up using initialize, which performs the transition into the initial state and sets up timers (if required).
  initialize()
}
