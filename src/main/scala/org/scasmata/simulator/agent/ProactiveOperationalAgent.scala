// Copyright (C) Maxime MORGE 2018
package org.scasmata.simulator.agent

import akka.actor.{FSM, Stash}
import org.scasmata.simulator._
import org.scasmata.simulator.agent.rule.ProactiveRule

/**
  * Proactive agent behaviour
  * @param id of its body
  */
class ProactiveOperationalAgent(id : Int) extends OperationalAgent(id) with ProactiveRule{

  /**
    * Handle message
    */
  override def receive : PartialFunction[Any,Unit] = {
    // If the perception is updated
    case Update(e) =>
      if (debug) println(s"Agent$id is updated")
      var targets = selectSingleTargets(id,e)
      //TODO whatif there is some heavy packets
      if (debug) println(s"Agent$id chooses target $targets")
      sender ! Inform(targets)
      val updatedMind = new Perception(e, mind.load, mind.attempt, targets)
      val nextInfluence = takeAction(id, updatedMind)
      if (debug) println(s"Agent$id decides $nextInfluence")
      sender ! nextInfluence
      mind = new Perception(e, mind.load, Some(nextInfluence), targets)

    // If the last influence is successful
    case Success =>
      if (debug) println(s"Agent$id is informed that its previous influence success")
      if (debug) println(s"Agent$id observes")
      sender ! Observe
      mind = mind.attempt match {
        case Some(PickUp(packet)) =>
          val newTarget = mind.targets match{
            case Nil => Nil
            case _ => mind.targets.tail
          }
          new Perception(mind.e, load = Some(packet), attempt = None, newTarget)
        case Some(PutDown(_)) =>
          new Perception(mind.e, load = None, attempt = None, mind.targets)
        case _ =>
          new Perception(mind.e, mind.load, attempt = None, mind.targets)
      }

    // If the previous influence is failed
    case Failure =>
      if (debug) println(s"Agent$id is informed that its previous influence succeed")
      if (debug) println(s"Agent$id observes")
      sender ! Observe
      mind = new Perception(mind.e, mind.load, null, mind.targets)
  }
}