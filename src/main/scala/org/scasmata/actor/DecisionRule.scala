// Copyright (C) Maxime MORGE 2018
package org.scasmata.actor

import scala.util.Random

import org.scasmata.environment.{Environment,Packet}

/**
  * Abstract decision rule
  */
trait DecisionRule{
  /**
    * Decide next move
    *
    * @param e the environment perceived
    * @param bodyId
    * @param load  packetId it owns, 0 otherwise
    */
  def decide(e: Environment, bodyId: Int, load: Int) : Influence
}

/**
  * Zero intelligent decision rule
  */
trait ZeroIntelligent extends DecisionRule{

  /**
    * Decide next move by:
    * 1. put done packet if possible
    * 2. pick up any packed if possible
    * 3. move randomly if possible
    */
  def decide(e: Environment, bodyId: Int, load: Int) : Influence = {
    val (i,j) = e.bodyLocation(bodyId)
    println(s"Agent$bodyId in ($i,$j) decides")
    val neighborhood = e.neighborhood(i,j)
    //1. put done packet if possible
    if (load != 0 && neighborhood.exists(c => c.hasDestination))
      return PutDown(load, e.colorPackets)
    //2. pick up any packed if possible
    if (load == 0) {
      neighborhood.foreach { c =>
        if (c.hasPacket) {
          val packetId = c.content.asInstanceOf[Packet].id
          return PickUp(packetId)
        }
      }
    }
    //3. move randomly if possible
    val random = Random
    val directions = e.possibleDirections(i,j)
    Move(directions(random.nextInt(directions.length)))
  }
}
