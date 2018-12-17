// Copyright (C) Maxime MORGE 2018
package org.scasmata.actor

import java.util.concurrent.ThreadLocalRandom

import org.scasmata.actor

import scala.util.Random
import org.scasmata.environment._

/**
  * Abstract decision rule
  */
trait DecisionRule{
  val debug = true
  val rnd : ThreadLocalRandom = ThreadLocalRandom.current()
  /**
    * Decide next move
    *
    * @param e the environment perceived
    * @param bodyId
    * @param load  packetId it owns, 0 otherwise
    */
  def decide(bodyId: Int, mind: Mind) : Influence
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
  def decide(bodyId: Int, mind: Mind) : Influence = {
    val (i,j) = mind.perception.bodyLocation(bodyId)
    println(s"Agent$bodyId in ($i,$j) decides")
    val neighborhood = mind.perception.neighborhood(i,j)
    //1. put done packet if possible
    if (mind.load != 0 && neighborhood.exists(c => c.hasDestination))
      return PutDown(mind.load, mind.perception.colorPackets)
    //2. pick up any packed if possible
    if (mind.load == 0) {
      neighborhood.foreach { c =>
        if (c.hasPacket) {
          val packetId = c.content.asInstanceOf[Packet].id
          return PickUp(packetId)
        }
      }
    }
    //3. move randomly if possible
    val random = Random
    val directions = mind.perception.possibleDirections(i,j)
    Move(directions(rnd.nextInt(directions.length)))
  }
}


/**
  * clever decision rule
  */
trait CleverWalk extends DecisionRule{

  /**
    * Select the targets
    */
  def selectTargets(id: Int, perception : Environment) : Seq[Int] = {
    val targets = perception.packetIds().filter(_ % perception.nbAgentBodies +1 == id)
    if (debug) println(s"Agent$id selects targets $targets")
    targets.toSeq
  }


  /**
    * Decide next move
    */
  def decide(bodyId: Int, mind: Mind) : Influence = {
    val (i,j) = mind.perception.bodyLocation(bodyId)
    println(s"Agent$bodyId in ($i,$j) decides")
    val neighborhood = mind.perception.neighborhood(i,j)

    if (mind.load != 0) {
      if (debug) println(s"Agent$bodyId is loaded")
      if (neighborhood.exists(c =>  mind.perception.hasDestinationForCell(c, mind.load))) {
        if (debug) println(s"Agent$bodyId is closed to the destination, put down packet")
        return PutDown(mind.load, mind.perception.colorPackets)
      }
      if (debug) println(s"Agent$bodyId move toward the destination")
      return moveToward( (i,j), mind.perception.destinationLocation(mind.perception.colorPacket(mind.load)))
    }
    //
    if (mind.targets.isEmpty){
      if (debug) print(s"Agent$bodyId has no target and so stay alive ahahah")
      return Move(Center)
    }
    val target = mind.targets.head
    if (debug) print(s"Agent$bodyId has target $target")
    if (neighborhood.exists(c => c.hasPacket(target))) {
      if (debug) println(s"Agent$bodyId picks $target since it is closed")
      return PickUp(target)
    }
    if (debug) println(s"Agent$bodyId  moves toward the target $target")
    moveToward( (i,j), mind.perception.packetLocation(target))
  }

  /**
    * Return the next move to go from source toward a destination
    */
  def moveToward(source: (Int,Int), destination : (Int,Int)) : Move = {
    val (xs,ys) = source
    val (xd,yd) = destination
    var directions = Seq[Direction]()
    if (xd>xs) directions :+= South
    if (xd<xs) directions :+= North
    if (yd>ys) directions :+= East
    if (yd<ys) directions :+= West
    val direction = directions(rnd.nextInt(directions.length))
    if (debug) println(s"move $direction to go from ($xs,$ys) to ($xd,$yd)")
    Move(direction)
  }
}
