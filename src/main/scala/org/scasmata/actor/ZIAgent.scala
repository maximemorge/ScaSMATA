// Copyright (C) Maxime MORGE 2018
package org.scasmata.actor

import java.util.concurrent.ThreadLocalRandom

import akka.actor.{Actor, ActorRef, FSM, Stash}
import org.scasmata.environment.{Center, Environment}

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
class Mind(val perception: Environment, val load: Int, val attempt: Influence, val targets: Seq[Int])


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
  startWith(Initial,  new Mind(null, 0, null, null))

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
  // Finally Triggering it up using initialize, which performs the transition into the initial state and sets up timers (if required).
  initialize()
}

/**
  * Agent behaviour with random walk
   * @param id of its body
  */
class ZIAgent(id : Int) extends Agent(id) with FSM[State, Mind]
  with Stash with ZeroIntelligent{

  /**
    * Either the agent is in the initial state
    */
  when(Initial) {
    // If the perception is updated
    case Event(Update(e), mind) =>
      val updatedMind = new Mind(e, mind.load, mind.attempt, null)
      if (debug) println(s"Agent$id is updated")
      val nextInfluence = decide(id, updatedMind)
      if (debug) println(s"Agent$id decides $nextInfluence")
      sender ! nextInfluence
      stay using new Mind(e, mind.load, nextInfluence, null)

      // If the last influence is successful
    case Event(Success, mind) =>
      if (debug) println(s"Agent$id is informed that its previous influence success")
      if (debug) println(s"Agent$id observes")
      sender ! Observe
      stay using (mind.attempt match {
        case PickUp(idPacket) => new Mind(mind.perception, idPacket, null, null)
        case PutDown(_,_) => new Mind(mind.perception, 0, null, null)
        case _ => new Mind(mind.perception, mind.load, null, null)
      })

      // If the previous influence is failed
    case Event(Failure, mind) =>
      if (debug) println(s"Agent$id is informed that its previous influence succeed")
      if (debug) println(s"Agent$id observes")
      sender ! Observe
      stay using new Mind(mind.perception, mind.load, null, null)
  }
}


/**
  * Agent behaviour with random walk
  * @param id of its body
  */
class CleverAgent(id : Int) extends Agent(id) with FSM[State, Mind]
  with Stash with CleverWalk{
  /**
    * Either the agent is in the initial state
    */
  when(Initial) {
    // If the perception is updated
    case Event(Update(e), mind) =>
      if (debug) println(s"Agent$id is updated")
      val updatedMind = new Mind(e, mind.load, mind.attempt, selectTargets(id,e))
      val nextInfluence = decide(id, updatedMind)
      if (debug) println(s"Agent$id decides $nextInfluence")
      sender ! nextInfluence
      stay using new Mind(e, mind.load, nextInfluence, selectTargets(id,e))

    // If the last influence is successful
    case Event(Success, mind) =>
      if (debug) println(s"Agent$id is informed that its previous influence failed")
      if (debug) println(s"Agent$id observes")
      sender ! Observe
      stay using (mind.attempt match {
        case PickUp(idPacket) =>
          val newTarget = mind.targets match{
            case Nil => Nil
            case _ => mind.targets.tail
          }
          new Mind(mind.perception, idPacket, null, newTarget)
        case PutDown(_,_) =>
          new Mind(mind.perception, 0, null, mind.targets)
        case _ =>
          new Mind(mind.perception, mind.load, null, mind.targets)
      })

    // If the previous influence is failed
    case Event(Failure, mind) =>
      if (debug) println(s"Agent$id is informed that its previous influence succeed")
      if (debug) println(s"Agent$id observes")
      sender ! Observe
      stay using new Mind(mind.perception, mind.load, null, mind.targets)
  }
}