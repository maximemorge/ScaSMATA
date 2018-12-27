// Copyright (C) Maxime MORGE 2018
package org.scasmata.simulator.agent

import akka.actor.{FSM, Stash}
import org.scasmata.simulator._
import org.scasmata.simulator.agent.rule.ReactiveRule

/**
  * Agent behaviour with random walk
  *
  * @param id of its body
  */
class ReactiveAgent(id : Int) extends Agent(id) with FSM[State, Mind]
  with Stash with ReactiveRule{

  /**
    * Either the agent is in the initial state
    */
  when(Initial){
    // If the perception is updated
    case Event(Update(e), mind) =>
      val updatedMind = new Mind(e, mind.load, mind.attempt, targets = Nil)
      if (debug) println(s"Agent$id is updated")
      val nextInfluence = decide(id, updatedMind)
      if (debug) println(s"Agent$id decides $nextInfluence")
      sender ! nextInfluence
      stay using new Mind(e, mind.load, Some(nextInfluence), targets = Nil)

    // If the last influence is successful
    case Event(Success, mind) =>
      if (debug) println(s"Agent$id is informed that its previous influence success")
      if (debug) println(s"Agent$id observes")
      sender ! Observe
      stay using (mind.attempt match {
        case Some(PickUp(packet)) => new Mind(mind.perception, load = Some(packet), attempt = None, targets = Nil)
        case Some(PutDown(_)) => new Mind(mind.perception, load = None , attempt = None, targets = Nil)
        case _ => new Mind(mind.perception, mind.load, attempt = None, targets = Nil)
      })

    // If the previous influence is failed
    case Event(Failure, mind) =>
      if (debug) println(s"Agent$id is informed that its previous influence succeed")
      if (debug) println(s"Agent$id observes")
      sender ! Observe
      stay using new Mind(mind.perception, mind.load, null, null)
  }

  // Finally triggering it up using initialize, which performs the transition into the initial state and sets up timers (if required).
  initialize()
}