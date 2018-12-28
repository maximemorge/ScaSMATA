// Copyright (C) Maxime MORGE 2018
package org.scasmata.simulator.agent

import org.scasmata.simulator._
import org.scasmata.simulator.agent.rule.ReactiveRule

/**
  * OperationalAgent behaviour performing a random walk
  * @param id of its body
  */
class ReactiveOperationalAgent(id : Int) extends OperationalAgent(id) with ReactiveRule{

  /**
    * Handle Influence messages
    */
  override def receive : PartialFunction[Any,Unit] = {
    // If the environment perception is updated
    case Delegate(_,e) =>
      perception = new Perception(e, perception.load, perception.attempt, None)
      if (debug) println(s"OperationalAgent$id is updated")
      val nextInfluence = takeAction(id, perception)
      if (debug) println(s"OperationalAgent$id decides $nextInfluence")
      perception = new Perception(e, perception.load, Some(nextInfluence), None)
      sender ! nextInfluence

    // If the last influence is successful
    case Success =>
      if (debug) println(s"OperationalAgent$id is informed that its previous influence success")
      if (debug) println(s"OperationalAgent$id observes")
      sender ! Observe
      perception = perception.attempt match {
        case Some(PickUp(packet)) =>
          new Perception(perception.e, load = Some(packet), attempt = None, None)
        case Some(PutDown(_)) =>
          new Perception(perception.e, load = None , attempt = None, None)
        case _ =>
          new Perception(perception.e, perception.load, attempt = None, None)
      }
    // If the previous influence is failed
    case Failure =>
      if (debug) println(s"OperationalAgent$id is informed that its previous influence succeed")
      if (debug) println(s"OperationalAgent$id observes")
      sender ! Observe
      perception = new Perception(perception.e, perception.load, attempt = None, None)
  }
}