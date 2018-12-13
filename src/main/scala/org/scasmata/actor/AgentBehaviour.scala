// Copyright (C) Maxime MORGE 2018
package org.scasmata.actor

import java.time.chrono.MinguoDate

import akka.actor.{Actor, ActorRef, FSM, Stash}

import scala.language.postfixOps
import org.scasmata.environment.{Center, Environment}

/**
  * States of the agent
  */
sealed trait State
case object Initial extends State

/**
  * Internal immutable state of mind
  * @param e its perception
  * @param load  packet id it bear, 0 otherwise
  */
class Mind(val e: Environment, val load : Int)

/**
  * Agent behaviour
   * @param id of the body
  */
class AgentBehaviour(id : Int) extends Actor with FSM[State, Mind] with Stash with ZI{
  val debug = true

  var scheduler: ActorRef = context.parent
  var directory: Directory = new Directory()

  /**
    * Initiates a myopic agent
    */
  startWith(Initial,  new Mind(null, 0))


  /**
    * Either the agent is in the initial state
    */
  when(Initial) {
    // If the perception is updated
    case Event(Update(e), mind) =>
      if (debug) println(s"Agent$id : it is updated about the environment")
      val newMind = new Mind(e, mind.load)
      val nextInfluence = decide(newMind.e, id, newMind.load)
      if (debug) println(s"Agent$id : it decides to stay")
      sender ! nextInfluence
      stay using newMind

    case Event(Success, mind) =>
      if (debug) println(s"Agent$id : last influence failed")
      //if (debug) println(s"Agent$id : it decides to stop")
      //sender ! Finished(0)
      if (debug) println(s"Agent$id : it observes")
      sender ! Observe
      stay using mind

    case Event(Failure, mind) =>
      if (debug) println(s"Agent$id : last influence successful")
      if (debug) println(s"Agent$id : it observes")
      sender ! Observe
      stay using mind

  }

  /**
    * Whatever the state is
    */
  whenUnhandled {
    // If the agent is initiated with the directory
    case Event(Init(d), mind) =>
      this.scheduler = sender
      this.directory = d
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
