// Copyright (C) Maxime MORGE 2018
package org.scasmata.actor

import akka.actor.{Actor, ActorRef, FSM, Stash}
import org.scasmata.environment.{Center, Environment}

/**
  * States of the agent
  */
sealed trait State
case object Initial extends State

/**
  * Internal immutable state of mind
  * @param e its perception
  * @param load  packetId it owns, 0 otherwise
  * @param expectedLoad packetId it try to load, 0 otherwise
  */
class Mind(val e: Environment,val load : Int,val attempt : Influence)

/**
  * Agent behaviour
   * @param id of its body
  */
class Agent(id : Int) extends Actor with FSM[State, Mind]
  with Stash with ZeroIntelligent{
  val debug = true

  var simulator: ActorRef = context.parent
  var directory: Directory = new Directory()
  var step = 0

  /**
    * Initiates a myopic agent which owns no packet
    */
  startWith(Initial,  new Mind(null, 0, null))


  /**
    * Either the agent is in the initial state
    */
  when(Initial) {
    // If the perception is updated
    case Event(Update(e), mind) =>
      if (debug) println(s"Agent$id is updated")
      val nextInfluence = decide(e, id, mind.load)
      if (!nextInfluence.isInstanceOf[Move] || nextInfluence.asInstanceOf[Move].direction != Center)
        step+=1
      if (debug) println(s"Agent$id decides $nextInfluence")
      sender ! nextInfluence
      stay using new Mind(e, mind.load, nextInfluence)

    case Event(Success, mind) =>
      if (debug) println(s"Agent$id is informed that its previous influence failed")
      if (debug) println(s"Agent$id observes")
      sender ! Observe
      stay using (mind.attempt match {
        case PickUp(idPacket) => new Mind(mind.e, idPacket, null)
        case PutDown(_,_) => new Mind(mind.e, 0, null)
        case _ => new Mind(mind.e, mind.load, null)
      })

    case Event(Failure, mind) =>
      if (debug) println(s"Agent$id is informed that its previous influence succeed")
      if (debug) println(s"Agent$id observes")
      sender ! Observe
      stay using new Mind(mind.e, mind.load, null)
  }

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

    // If the agent is stopped at the end of the run
    case Event(QueryResult,mind) =>
      if (debug) println(s"Agent$id is stopped")
      sender ! Result(step)
      context.stop(self)
      stay using mind

    // If the agent is stopped befor the end of the run
    case Event(Kill,mind) =>
      context.stop(self)
      stay using mind

    // In case of unexpected event
    case Event(e, mind) =>
      println(s"Agent$id has received an unexpected event {} in state {}/{}", e, stateName, mind)
      stay using mind

  }

  // Finally Triggering it up using initialize, which performs the transition into the initial state and sets up timers (if required).
  initialize()
}
