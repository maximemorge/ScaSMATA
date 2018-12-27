// Copyright (C) Maxime MORGE 2018
package org.scasmata.simulator.agent.rule

import org.scasmata.simulator._
import org.scasmata.environment.Packet
import org.scasmata.simulator.agent.Mind

/**
  * Reactive decision rule
  */
trait ReactiveRule extends DecisionRule{
  /**
    * Decide next move by:
    * 1. put done packet if possible
    * 2. pick up any packed if possible
    * 3. move randomly if possible
    */
  def decide(id: Int, mind: Mind) : Influence = {
    val (i,j) = mind.perception.location(mind.perception.bodies(id))
    println(s"Agent$id in ($i,$j) decides")
    val neighborhood = mind.perception.neighborhood(i,j)
    //1. put done packet if possible
    if (mind.load.isDefined && neighborhood.exists(c => c.hasDestination))
      return PutDown(mind.load.get)
    //2. pick up any packed if possible
    if (mind.load.isEmpty) {
      neighborhood.foreach { c =>
        if (c.hasPacket) {
          val packet = c.content.asInstanceOf[Packet]
          return PickUp(packet)
        }
      }
    }
    //3. move randomly if possible
    val directions = mind.perception.possibleDirections(i,j)
    Move(directions(rnd.nextInt(directions.length)))
  }
}
