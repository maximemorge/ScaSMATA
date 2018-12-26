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
    val (i,j) = mind.perception.location(mind.perception.bodies(bodyId))
    println(s"Agent$bodyId in ($i,$j) decides")
    val neighborhood = mind.perception.neighborhood(i,j)
    //1. put done packet if possible
    if (mind.load.isDefined && neighborhood.exists(c => c.hasDestination))
      return PutDown(mind.load.get)
    //2. pick up any packed if possible TODO only the target
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


/**
  * clever decision rule
  */
trait CleverWalk extends DecisionRule{

  /**
    * Select the targets
    */
  def selectUnitTargets(id: Int, perception : Environment) : Seq[Packet] = {
    val targets = perception.packetsOfSize(size = 1).filter(_.id % perception.n +1 == id)
    if (debug) println(s"Agent$id selects targets $targets")
    targets.toSeq
  }


  /**
    * Decide next move
    */
  def decide(bodyId: Int, mind: Mind) : Influence = {
    val body = mind.perception.bodies(bodyId)
    val (i,j) = mind.perception.location(body)
    println(s"Agent$bodyId in ($i,$j) decides")
    val neighborhood = mind.perception.neighborhood(i,j)

    if (mind.load.isDefined) {
      if (debug) println(s"Agent$bodyId is loaded")
      if (mind.perception.closedDestination(body)) {
        if (debug) println(s"Agent$bodyId is closed to the destination, put down packet")
        return PutDown(mind.load.get)
      }
      if (debug) println(s"Agent$bodyId move toward the destination ${mind.perception.destinationLocation()}")
      return moveToward( (i,j), mind.perception.destinationLocation(), mind.perception)
    }
    //
    if (mind.targets.isEmpty){
      if (debug) println(s"Agent$bodyId has no target and so stay alive ahahah")
      return Move(Center)
    }
    val target = mind.targets.head
    if (debug) println(s"Agent$bodyId has target $target ${mind.perception.location(target)}")
    if (mind.perception.closedPacket(body,target)){
      if (debug) println(s"Agent$bodyId picks $target since it is closed")
      return PickUp(target)
    }
    if (debug) println(s"Agent$bodyId  moves toward the target $target")
    moveToward( (i,j), mind.perception.location(target), mind.perception)
  }

  /**
    * Return the next move to fly from source toward a destination (according to euclidean distance)
    */
  def flyToward(source: (Int,Int), destination : (Int,Int), e : Environment) : Move = {
    val (xs,ys) = source
    val (xd,yd) = destination
    var directions = Seq[Direction]()
    if (xd>xs) directions :+= South
    if (xd<xs) directions :+= North
    if (yd>ys) directions :+= East
    if (yd<ys) directions :+= West
    val direction = directions(rnd.nextInt(directions.length))
    Move(direction)
  }

  /**
    * Return the next move to go from source toward a destination (according to  Dijkstra's algorithm)
    */
  def moveToward(source: (Int,Int), destination : (Int,Int), e : Environment) : Move = {
    val (xs,ys) = source
    val (xd,yd) = destination
    val dijkstra = new Dijkstra(e,xs,ys)
    dijkstra.run()
    Move(dijkstra.nextDirectionTo(xd,yd))
  }


}
