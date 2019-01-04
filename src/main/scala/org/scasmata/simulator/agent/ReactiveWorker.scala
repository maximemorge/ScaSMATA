// Copyright (C) Maxime MORGE 2018
package org.scasmata.simulator.agent

import org.scasmata.simulator._
import org.scasmata.simulator.agent.rule.ReactiveRule

/**
  * Worker behaviour performing a random walk
  * @param id of its body
  */
class ReactiveWorker(id : Int) extends OperationalAgent(id) with ReactiveRule{

  /**
    * Handle Influence messages
    */
  override def receive : PartialFunction[Any,Unit] = {
    // The environment perception is updated
    case Delegate(_,e) =>
      perception = new Perception(e, perception.load, perception.attempt, None)
      if (debug) println(s"Worker$id is updated")
      val nextInfluence = takeAction(id, perception)
      if (debug) println(s"Worker$id decides $nextInfluence")
      perception = new Perception(e, perception.load, Some(nextInfluence), None)
      sender ! nextInfluence
    // The last influence is successful
    case Success =>
      if (debug) println(s"Worker$id is informed that its previous influence ${perception.attempt} success")
      perception = perception.attempt match {
        case Some(Merge(_)) =>
          if (debug) println(s"Worker$id commits suicide")
          sender ! Kill
          new Perception(perception.e, perception.load, attempt = None, None)
        case Some(PickUp(packet)) =>
          if (debug) println(s"Worker$id observes")
          sender ! Observe
          new Perception(perception.e, load = Some(packet), attempt = None, None)
        case Some(PutDown(_)) =>
          if (debug) println(s"Worker$id observes")
          sender ! Observe
          new Perception(perception.e, load = None , attempt = None, None)
        case Some(Move(_)) =>
          if (debug) println(s"Worker$id observes")
          sender ! Observe
          new Perception(perception.e, perception.load, attempt = None, None)
        case None =>
          throw new RuntimeException(s"Worker$id does not understand success of None")
      }
    // The previous influence is failed
    case Failure =>
      if (debug) println(s"Worker$id is informed that its previous influence ${perception.attempt} failed")
      if (debug) println(s"Worker$id observes")
      sender ! Observe
      perception = new Perception(perception.e, perception.load, attempt = None, None)
  }
}