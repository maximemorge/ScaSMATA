// Copyright (C) Maxime MORGE 2018
package org.scasmata.environment

/**
  * Entity in the environment are bodies, packets, destination or nothing (if the cell is empty)
  */
abstract class Entity
case class Destination()
  extends Entity{
  override def toString: String = s"D   "
}

/**
  * An agent body has an id and it carries a packet or none
  * @param id
  * @param load
  */
case class AgentBody(id: Int, var load: Option[Packet] = None) extends Entity{
  override def toString: String = "B"+id+load

  /**
    * Returns the cost of the current load
    */
  def charge() : Int = load match {
    case Some(packet) => packet.size
    case None => 0
  }

  /**
    * Carry a packet
    */
  def take(packet: Packet) : Unit = {
    load = Some(packet)
  }

  /**
    * Unload the packet it carries
    */
  def unload() : Unit = {
    load = None
  }

  /**
    *
    */
  override def equals(that: Any): Boolean = {
    that match {
      case that: AgentBody => that.id == this.id
      case _ => false
    }
  }
}

/**
  * A packet has an id and a size, eventually a color if it is targeted by an agent
  */
case class Packet(id: Int, size: Int, var color: Color = Brown) extends Entity{
  override def toString: String = s"P$id$size$color"
  override def equals(that: Any): Boolean = {
    that match {
      case that: Packet => that.id == this.id
      case _ => false
    }
  }
}