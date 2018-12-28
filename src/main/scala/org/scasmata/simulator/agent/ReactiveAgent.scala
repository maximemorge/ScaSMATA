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
class ReactiveAgent(id : Int) extends Agent(id) with ReactiveRule{

  /**
    * handle message
    */
  override def receive = {
    // If the perception is updated
    case Update(e) =>
      val updatedMind = new Mind(e, mind.load, mind.attempt, targets = Nil)
      if (debug) println(s"Agent$id is updated")
      val nextInfluence = decide(id, updatedMind)
      if (debug) println(s"Agent$id decides $nextInfluence")
      sender ! nextInfluence
      mind = new Mind(e, mind.load, Some(nextInfluence), targets = Nil)

    // If the last influence is successful
    case Success =>
      if (debug) println(s"Agent$id is informed that its previous influence success")
      if (debug) println(s"Agent$id observes")
      sender ! Observe
      mind = mind.attempt match {
        case Some(PickUp(packet)) => new Mind(mind.perception, load = Some(packet), attempt = None, targets = Nil)
        case Some(PutDown(_)) => new Mind(mind.perception, load = None , attempt = None, targets = Nil)
        case _ => new Mind(mind.perception, mind.load, attempt = None, targets = Nil)
      }

    // If the previous influence is failed
    case Failure =>
      if (debug) println(s"Agent$id is informed that its previous influence succeed")
      if (debug) println(s"Agent$id observes")
      sender ! Observe
      mind = new Mind(mind.perception, mind.load, null, null)
  }
}