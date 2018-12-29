// Copyright (C) Maxime MORGE 2018
package org.scasmata.simulator.agent.rule

import org.scasmata.simulator._
import org.scasmata.environment.Packet
import org.scasmata.simulator.agent.Perception

/**
  * Reactive decision rule
  */
trait ReactiveRule extends OperationalRule{
  /**
    * Decide next move by:
    * 1. put done packet if possible
    * 2. pick up any packed if possible
    * 3. move randomly if possible
    */
  def takeAction(id: Int, perception: Perception) : Influence = {
    val (i,j) = perception.e.location(perception.e.bodies(id))
    println(s"Agent$id in ($i,$j) decides")
    val neighborhood = perception.e.neighborhood(i,j)
    //1. put done packet if possible
    if (perception.load.isDefined && neighborhood.exists(c => c.hasDestination))
      return PutDown(perception.load.get)
    //2. pick up any packed if possible
    if (perception.load.isEmpty) {
      neighborhood.foreach { c =>
        if (c.hasPacket) {
          val packet : Packet= c.content.get.asInstanceOf[Packet]
          return PickUp(packet)
        }
      }
    }
    //3. move randomly if possible
    val directions = perception.e.emptyDirection(i,j)
    Move(directions(rnd.nextInt(directions.length)))
  }
}
