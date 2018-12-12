// Copyright (C) Maxime MORGE 2018
package org.scasmata.actor

import akka.actor.ActorRef
import org.scasmata.environment.AgentBody

/**
  * Class representing an index of the names and addresses of agents
  */
class Directory {
  var adr = Map[AgentBody, ActorRef]()//Bodies' references
  var bodies = Map[ActorRef, AgentBody]()// Actors' body

  override def toString: String = allBodies().mkString("[",", ","]")

  /**
    * Add to the directory
    * @param body
    * @param ref
    */
  def add(body: AgentBody, ref: ActorRef) : Unit = {
    if ( ! adr.keySet.contains(body) &&  ! bodies.keySet.contains(ref)) {
      adr += (body -> ref)
      bodies += (ref -> body)
    }
    else throw new RuntimeException(s"$body and/or $ref already in the directory")
  }

  def allActors() : Iterable[ActorRef]  = adr.values
  def allBodies() : Iterable[AgentBody]  = bodies.values
  def peers(worker: AgentBody) : Set[AgentBody] = allBodies().filterNot(_ ==worker).toSet
  def peersActor(worker: AgentBody) :  Iterable[ActorRef] = peers(worker: AgentBody).map(w => adr(w))

}
