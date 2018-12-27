// Copyright (C) Maxime MORGE 2018
package org.scasmata.simulator.agent.rule

import org.scasmata.simulator._
import org.scasmata.environment._
import org.scasmata.simulator.agent.Mind

/**
  * Proactive decision rule
  */
trait ProactiveRule extends DecisionRule{
  /**
    * Select the targets according to the agent id and the perception
    */
  def selectSingleTargets(id: Int, perception : Environment) : Seq[Packet] = {
    val targets = perception.packetsOfSize(size = 1).filter(_.id % perception.n +1 == id)
    if (debug) println(s"Agent$id selects targets $targets")
    targets.toSeq
  }

  /**
    * Decides next move of the agent id and its mind
    */
  def decide(id: Int, mind: Mind) : Influence = {
    val body = mind.perception.bodies(id)
    val (i,j) = mind.perception.location(body)
    println(s"Agent$id in ($i,$j) decides")
    if (mind.load.isDefined) {
      if (debug) println(s"Agent$id is loaded")
      if (mind.perception.closedDestination(body)) {
        if (debug) println(s"Agent$id is closed to the destination, put down packet")
        return PutDown(mind.load.get)
      }
      val destination =  mind.perception.destinationLocation()
      if (debug) println(s"Agent$id move toward the destination $destination")
      return moveToward( (i,j), destination, mind.perception)
    }
    if (mind.targets.isEmpty){
      if (debug) println(s"Agent$id has no target and so stay alive")
      return Move(Center)
    }
    val target = mind.targets.head
    val place = mind.perception.location(target)
    if (debug) println(s"Agent$id has target $target in $place")
    if (mind.perception.closedPacket(body,target)){
      if (debug) println(s"Agent$id picks $target since it is closed")
      return PickUp(target)
    }
    if (debug) println(s"Agent$id moves toward the target $target")
    moveToward( (i,j), place , mind.perception)
  }

  /**
    * Returns the next move to fly from source toward a destination, i.e
    * according to euclidean distance, i.e. without Dijkstra algorithm
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
    * Return the next move to go from source toward a destination, i.e.
    * according to  Dijkstra's algorithm
    */
  def moveToward(source: (Int,Int), destination : (Int,Int), e : Environment) : Move = {
    val (xs,ys) = source
    val (xd,yd) = destination
    val dijkstra = new Dijkstra(e,xs,ys)
    dijkstra.run()
    Move(dijkstra.nextDirectionTo(xd,yd))
  }
}
