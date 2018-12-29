// Copyright (C) Maxime MORGE 2018
package org.scasmata.simulator.agent

import akka.actor.{Actor, ActorRef, Props}
import org.scasmata.environment.{Environment, Packet}
import org.scasmata.simulator._

/**
  * Agent behaviour which plans targeted packets
  * and delegates them to the operational agent
  * @param id of the corresponding body
  */

class StrategicAgent(val id : Int) extends Actor {
  val debug = true

  var simulator: ActorRef = context.parent
  var directory: Directory = new Directory()
  var operationalAgent : ActorRef= context.actorOf(Props(classOf[ProactiveOperationalAgent], id), id.toString)
  var vision: Environment = _

  /**
    * Select the single targets (lightweight packets) according to the agent id and the perception
    */
  var singleTargets = Seq[Packet]()

  /**
    * Select the multi targets (heavy packets) according to the agent id and the perception
    */
  var multiTargets = Seq[Packet]()

  /**
    * Compute single/multi targets
    */
  def takeAim() : Unit ={
    singleTargets =vision.lightweightPackets().filter(_.id % vision.n +1 == id).toSeq
    multiTargets = vision.heavyPackets().filter(_.id % vision.n +1 == id).toSeq
  }



  /**
    * Handle messages
    */
  override def receive : PartialFunction[Any,Unit] = process orElse forward

  /**
    * Processes message
    */
  def process : PartialFunction[Any,Unit] = {
    // If the strategicAgent is initiated with the directory
    case Init(d) =>
      this.directory = d
      if (debug) println(s"StrategicalAgent$id is ready")
      sender ! Ready
    // If the perception is updated
    case Update(e) =>
      this.simulator = sender
      if (debug) println(s"StrategicAgent$id is updated")
      vision = e
      takeAim()
      //TODO what if there is some heavy packets
      if (debug) println(s"StrategicAgent$id chooses target $singleTargets")
      sender ! Inform(singleTargets)
      operationalAgent ! Delegate(singleTargets.headOption,vision)
  }

  /**
    * Forwards message
    */
  def forward : PartialFunction[Any,Unit] = {
    // If the operational agent observes
    case Observe =>
      this.simulator ! Observe
    // If the operational agent acts
    case influence : Influence =>
      this.simulator ! influence
    // If the environment reacts
    case reaction: Reaction =>
      operationalAgent ! reaction
    // If the agent is killed
    case Kill =>
      operationalAgent ! Kill
      context.stop(self)
    // In case of unexpected event
    case message =>
      println(s"StrategicalAgent$id has received an unexpected $message")
  }
}
