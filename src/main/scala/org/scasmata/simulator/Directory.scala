// Copyright (C) Maxime MORGE 2018
package org.scasmata.simulator

import akka.actor.ActorRef

/**
  * Class representing an index of the names and addresses of agents
  */
class Directory {
  var adr = Map[Int, ActorRef]()//Bodies' references
  var id = Map[ActorRef, Int]()// Actors' body

  override def toString: String = allIds().mkString("[",", ","]")

  /**
    * Add to the directory a (bodyId,ref)
    */
  def add(bodyId: Int, ref: ActorRef) : Unit = {
    if ( ! adr.keySet.contains(bodyId) &&  ! id.keySet.contains(ref)) {
      adr += (bodyId -> ref)
      id += (ref -> bodyId)
    }
    else throw new RuntimeException(s"$bodyId and/or $ref already in the directory")
  }

  /**
    * Remove to the directory a bodyId
    */
  def remove(bodyId: Int, ref: ActorRef) : Unit = {
    if (adr.keySet.contains(bodyId) &&  id.keySet.contains(ref)) {
      adr -= bodyId
      id -= ref
    }
    else throw new RuntimeException(s"$bodyId and/or $ref are not in the directory")
  }

  def allAgents() : Iterable[ActorRef]  = adr.values
  def allIds() : Iterable[Int]  = id.values
  def peers(bodyId: Int) : Set[Int] = allIds().filterNot(_ == bodyId).toSet
  def peersActor(bodyId: Int) :  Iterable[ActorRef] = peers(bodyId).map(id => adr(id))
}
