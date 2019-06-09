// Copyright (C) Maxime MORGE 2018
package org.scasmata.simulator.agent

import akka.actor.{Actor, ActorRef, Props}

import org.scasmata.environment.Environment
import org.scasmata.simulator._
import org.scasmata.util.{Behaviour,Proactive,Reactive}

/**
  * Agent behaviour managing an ActiveEntity
  * which orchestrates the worker and the negotiator
  * @param id of the corresponding active entity
  * @param behaviour of the operational agent
*/
class Agent(val id : Int, val behaviour: Behaviour) extends Actor {
  val debug = false

  var simulator: ActorRef = context.parent
  var directory: Directory = new Directory()
  var worker : ActorRef= behaviour match {
    case Proactive => context.actorOf(Props(classOf[ProactiveOperationalAgent], id), "worker"+id.toString)
    case Reactive => context.actorOf(Props(classOf[ReactiveOperationalAgent], id), "worker"+id.toString)
  }

  var vision: Environment = _

  /**
    * Handle messages
    */
  override def receive : PartialFunction[Any,Unit] = process orElse forward

  /**
    * Processes message
    */
  def process : PartialFunction[Any,Unit] = {
    //The agent is initiated with the directory
    case Init(d) =>
      this.simulator = sender
      this.directory = d
      if (debug) println(s"Agent$id inits its negotiator")
      simulator ! Ready
    // The perception is updated
    case Update(e,targets) =>
      this.simulator = sender
      if (debug) println(s"Agent$id is updated")
      vision = e
      val nexTarget = targets.headOption
      if (nexTarget.isEmpty || nexTarget.get.weight == 1){
        if (debug) println(s"Agent$id has a feasible target $nexTarget.get")
        worker ! Delegate(nexTarget,vision)
      }else{
        if (debug) println(s"Agent$id has no more feasible target")
        worker ! Delegate(None,vision)
      }
  }

  /**
    * Forwards message
    */
  def forward : PartialFunction[Any,Unit] = {
    // The worker observes
    case Observe =>
      this.simulator ! Observe
    // The worker acts
    case influence : Influence =>
      this.simulator ! influence
    // The environment reacts
    case reaction: Reaction =>
      worker ! reaction
    // The agent is killed
    case Kill =>
      worker ! Kill
      context.stop(self)
    // In case of unexpected event
    case message =>
      println(s"WARNING: Agent$id has received an unexpected $message")
  }
}
