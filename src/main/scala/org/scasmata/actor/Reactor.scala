// Copyright (C) Maxime MORGE 2018
package org.scasmata.actor

import akka.actor.Actor
import org.scasmata.environment.Environment

/**
  * Reactor calculates, according to a set of domain specific laws,
  * the reaction, i.e. state changes in the environment
  */
trait Reactor extends Actor {
  val debug = true

  /**
    * Process each influence according to the physical laws of the environment
    * @param influences
    * @param e
    * @param directory
    */
  def react(influences: Map[Int,Influence], e : Environment, directory : Directory) : Unit = {
    influences.map{

      case (bodyId,Move(direction)) =>
        if (!e.isPossibleDirection(bodyId,direction)){
          if (debug) println(s"Move($direction) of $bodyId is impossible")
          directory.adr(bodyId) ! Failure
        }
        else{
          e.updateMove(bodyId,direction)
          if (debug) println(s"Move($direction) of $bodyId is performed")
          directory.adr(bodyId) ! Success
        }

      case (bodyId,PickUp(id)) =>
        val listOfPacket = e.closedPackets(bodyId)
        if (e.load(bodyId) != 0 || listOfPacket.isEmpty) {
          if (debug) println(s"Pickup($id) of $bodyId is failed")
          directory.adr(bodyId) ! Failure
        }
        else {
          val idPacket = listOfPacket.head
          e.updatePickUp(bodyId,idPacket)
          if (debug) println(s"Pickup($idPacket) of $bodyId is performed")
          directory.adr(bodyId) ! Success
        }

      case (bodyId, PutDown(idPacket,color)) =>
        val listOfDestination = e.closedDestinations(bodyId,color)
        if (e.load(bodyId)==0 || listOfDestination.isEmpty) {
          if (debug) println(s"PutDown($idPacket,$color) of $bodyId is failed")
          directory.adr(bodyId) ! Failure
        }
        else {
          e.updatePutDown(bodyId,idPacket)
          if (debug) println(s"PutDown($idPacket) of $bodyId is performed")
          directory.adr(bodyId) ! Success
          if (e.nbScatteredPackets == 0){
            if (debug) println(s"No more packets")
            stopAll(directory)
          }
          if (debug) println(s"There is still packets")
        }
      case (bodyId,influence) =>
        new RuntimeException(s"Reactor: $influence by $bodyId was not excepted")
    }
  }

  def stopAll(wP : Directory) = {
    wP.allAgents().foreach( _ ! QueryResult)
  }

}
