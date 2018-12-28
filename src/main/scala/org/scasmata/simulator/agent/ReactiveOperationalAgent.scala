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
class ReactiveOperationalAgent(id : Int) extends OperationalAgent(id) with ReactiveRule{

  /**
    * handle message
    */
  override def receive : PartialFunction[Any,Unit] = {
    // If the perception is updated
    case Update(e) =>
      val updatedMind = new Perception(e, mind.load, mind.attempt, targets = Nil)
      if (debug) println(s"Agent$id is updated")
      val nextInfluence = takeAction(id, updatedMind)
      if (debug) println(s"Agent$id decides $nextInfluence")
      sender ! nextInfluence
      mind = new Perception(e, mind.load, Some(nextInfluence), targets = Nil)

    // If the last influence is successful
    case Success =>
      if (debug) println(s"Agent$id is informed that its previous influence success")
      if (debug) println(s"Agent$id observes")
      sender ! Observe
      mind = mind.attempt match {
        case Some(PickUp(packet)) => new Perception(mind.e, load = Some(packet), attempt = None, targets = Nil)
        case Some(PutDown(_)) => new Perception(mind.e, load = None , attempt = None, targets = Nil)
        case _ => new Perception(mind.e, mind.load, attempt = None, targets = Nil)
      }
    // If the previous influence is failed
    case Failure =>
      if (debug) println(s"Agent$id is informed that its previous influence succeed")
      if (debug) println(s"Agent$id observes")
      sender ! Observe
      mind = new Perception(mind.e, mind.load, null, null)
  }
}