// Copyright (C) Maxime MORGE 2018
package org.scasmata.simulator.agent

import akka.actor.{Actor, ActorRef, Props}
import org.scasmata.environment.Environment
import org.scasmata.simulator._

/**
  * Agent behaviour managing an ActiveEntity
  * which orchestrates the worker and the negotiator
  * @param id of the corresponding active entity
*/
class Agent(val id : Int) extends Actor {
  val debug = true

  var simulator: ActorRef = context.parent
  var directory: Directory = new Directory()
  var worker : ActorRef= context.actorOf(Props(classOf[ProactiveWorker], id), "worker"+id.toString)
  var negotiator : ActorRef= context.actorOf(Props(classOf[Negotiator], id), "negotiator"+id.toString)

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
      negotiator ! Init(d)
    // The negotiator is ready
    case Ready =>
      simulator ! Ready
    // The perception is updated
    case Update(e) =>
      this.simulator = sender
      if (debug) println(s"Agent$id is updated")
      vision = e
      negotiator ! QueryTargets(vision)
    // The negotiator has chosen a target
    case ReplyTargets(targets) =>
      this.simulator ! Inform(targets)
      val nexTarget = targets.headOption
      if (nexTarget.isEmpty || nexTarget.get.size == 1){
        worker ! Delegate(nexTarget,vision)
      }else{
        //TODO
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
      negotiator ! Kill
      context.stop(self)
    // In case of unexpected event
    case message =>
      println(s"WARNING: Agent$id has received an unexpected $message")
  }
}
