// Copyright (C) Maxime MORGE 2018
package org.scasmata.actor

import akka.actor.{Actor, ActorRef, FSM, Props}
import org.scasmata.environment.{AgentBody, Center, East, Environment, North, South, West}

import scala.concurrent.Await

/**
  * Reactor calculates, according to a set of domain specific laws,
  * the reaction, i.e. state changes in the environment
  */
trait Reactor extends Actor {
  val debug = true

  def react(influences: Map[AgentBody,Influence], e : Environment, wP : Directory) : Unit = {
    influences.map{
      case (body,Move(d)) =>
        if (!e.isPossibleDirection(body.id,d)){
          if (debug) println(s"Move($d) of $body is impossible")
          wP.adr(body) ! Failure
        }
        else{
          e.updateMove(body.id,d)
          if (debug) println(s"Move($d) of $body is performed")
          wP.adr(body) ! Success
        }

      case (body,PickUp(id)) =>
        val listOfPacket = e.closedPackets(body.id)
        if (e.load(body.id)==0 || listOfPacket.isEmpty) {
          if (debug) println(s"Pickup($id) of $body is failed")
          wP.adr(body) ! Failure
        }
        else {
          val packetId = listOfPacket.head
          e.updatePickUp(body.id,packetId)
          if (debug) println(s"Pickup($id) of $body is performed")
          wP.adr(body) ! Success
        }

      case (body, PutDown(idPacket,color)) =>
        val listOfDestination = e.closedDestinations(body.id,color)
        if (e.load(body.id)==0 || listOfDestination.isEmpty) {
          if (debug) println(s"PutDown($idPacket,$color) of $body is failed")
          wP.adr(body) ! Failure
        }
        else {
          val packetId = listOfDestination.head
          e.updatePutDown(body.id,idPacket)
          if (debug) println(s"PutDown($idPacket) of $body is performed")
          wP.adr(body) ! Success
          if (e.nbAvailablePackets == 0){
            if (debug) println(s"No more packets")
            stopAll(wP)
          }
          if (debug) println(s"There is still packets")
        }
      case (body,influence) =>
        new RuntimeException(s"Reactor: $influence by ${body.id} was not excepted")
    }
  }

  def stopAll(wP : Directory) = {
    wP.allActors().foreach( _ ! QueryResult)
  }

}
