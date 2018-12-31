// Copyright (C) Maxime MORGE 2018
package org.scasmata.simulator.agent

import akka.actor.Actor
import org.scasmata.environment.{Environment, Packet}
import org.scasmata.simulator._

/**
  * Agent behaviour which plans targeted packets
  * @param id of the corresponding body
  */

class Negotiator(val id : Int) extends Actor {
  val debug = true

  var directory: Directory = _
  var vision : Environment = _

  /**
    * Select the single targets (lightweight packets) according to the agent id and the perception
    */
  var singleTargets = Seq[Packet]()

  /**
    * Select the multi targets (heavy packets) according to the agent id and the perception
    */
  var multiTargets = Seq[Packet]()

  /**
    * Handle messages
    */
  override def receive : PartialFunction[Any,Unit] ={
    // The negotiator is initiated with the directory
    case Init(d) =>
      this.directory = d
      if (debug) println(s"Negotiator$id is ready")
      sender ! Ready
    // The negotiator must choose a target
    case QueryTargets(e) =>
        vision = e
        singleTargets =vision.lightweightPackets().filter(_.id % vision.n +1 == id).toSeq
        multiTargets = vision.heavyPackets().filter(_.id % vision.n +1 == id).toSeq
        sender ! ReplyTargets(singleTargets)
    // The negotiator is killed
    case Kill =>
      context.stop(self)
    case message =>
      println(s"Negotiator$id has received an unexpected $message")
  }
}