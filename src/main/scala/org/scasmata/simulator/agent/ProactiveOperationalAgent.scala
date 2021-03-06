// Copyright (C) Maxime MORGE 2018
package org.scasmata.simulator.agent

import org.scasmata.simulator._
import org.scasmata.simulator.agent.rule.ProactiveRule

/**
  * Proactive operational agent behaviour
  * @param id of its activeEntity
  */
class ProactiveOperationalAgent(id : Int) extends OperationalAgent(id) with ProactiveRule{

  /**
    * Handle message
    */
  override def receive : PartialFunction[Any,Unit] = {
    // If the perception is updated
    case Delegate(t,e) =>
      if (debug) println(s"Worker$id is in charge of $t")
      perception = new Perception(e, perception.load, perception.attempt,t)
      val nextInfluence = takeAction(id, perception)
      if (debug) println(s"Worker$id decides $nextInfluence")
      sender ! nextInfluence
      perception = new Perception(e, perception.load, Some(nextInfluence),t)

    // If the last influence is successful
    case Success =>
      if (debug) println(s"Worker$id is informed that its previous influence ${perception.attempt} success")
      perception = perception.attempt match {
        case Some(PickUp(packet)) =>
          if (debug) println(s"Worker$id observe")
          sender ! Observe
          new Perception(perception.e, load = Some(packet), attempt = None, perception.target)
        case Some(PutDown(_)) =>
          if (debug) println(s"Worker$id observe")
          sender ! Observe
          new Perception(perception.e, load = None, attempt = None, None)
        case Some(Move(_)) =>
          if (debug) println(s"Worker$id observe")
          sender ! Observe
          new Perception(perception.e, perception.load, attempt = None, perception.target)
        case _ =>
          throw new RuntimeException(s"Worker$id should have memorized an attempt since it successes")
      }

    // If the previous influence is failed
    case Failure =>
      if (debug) println(s"Worker$id is informed that its previous influence ${perception.attempt} succeed")
      if (debug) println(s"Worker$id observes")
      sender ! Observe
  }
}