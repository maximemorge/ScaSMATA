// Copyright (C) Maxime MORGE 2018
package org.scasmata.simulator.agent

import akka.actor.Actor
import org.scasmata.environment.{Environment, Packet, ActiveEntity}
import org.scasmata.simulator._

/**
  * Negotiaor which plans targeted packets
  * @param id of the corresponding body
  */

class Negotiator(val id : Int) extends Actor {
  val debug = false

  var directory: Directory = _
  var vision : Environment = _

  /**
    * Handle messages
    */
  override def receive : PartialFunction[Any,Unit] ={
    // When the negotiator is initiated with the directory
    case Init(d) =>
      this.directory = d
      if (debug) println(s"Negotiator$id is ready")
      sender ! Ready
    // When targets are asked to the negotiator
    case QueryTargets(e) =>
        vision = e
        val targets=vision.packets.values.filter{ p : Packet =>
          p.id % vision.n +1 == id && p.weight <= e.activeEntities(id).capacity
        }.toSeq
        sender ! ReplyTargets(targets)
    // The negotiator is killed
    case Kill =>
      context.stop(self)
    case message =>
      println(s"Negotiator$id has received an unexpected $message")
  }
}