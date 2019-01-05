// Copyright (C) Maxime MORGE 2018
package org.scasmata.simulator.agent.rule

import org.scasmata.simulator._
import org.scasmata.environment.{ActiveEntity, Body, Packet}
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
    val entity = perception.e.activeEntities(id)
    val (i,j) = perception.e.location(entity)
    println(s"Agent$id in ($i,$j) decides")
    val neighborhood = perception.e.neighborhood(i,j)
    //1. put done packet if possible
    if (perception.load.isDefined && neighborhood.exists(c => c.hasDestination))
      return PutDown(perception.load.get)
    //2. merge with another body exists
    if (perception.load.isEmpty && neighborhood.exists(c => c.hasActiveEntity)){
      neighborhood.foreach { c =>
        if (c.hasActiveEntity) {
          val e : ActiveEntity = c.content.get.asInstanceOf[ActiveEntity]
          return Merge(e)
        }
      }
    }
    //3. pick up any packed if possible
    if (perception.load.isEmpty) {
      neighborhood.foreach { c =>
        if (c.hasPacket) {
          val packet : Packet= c.content.get.asInstanceOf[Packet]
          if (packet.size <=  entity.capacity) return PickUp(packet)
        }
      }
    }
    //4. move randomly if possible
    val directions = perception.e.accessibleDirections(i,j)
    Move(directions(rnd.nextInt(directions.length)))
  }
}
