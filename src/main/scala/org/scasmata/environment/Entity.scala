// Copyright (C) Maxime MORGE 2018
package org.scasmata.environment

/**
  * Entity in the environment are bodies, packets, destination or nothing (if the cell is empty)
  */
abstract class Entity
object Entity{
  val size = 7 // to print
}

/**
  * Destination where to put packets
  */
case class Destination() extends Entity{
  override def toString: String = "D"
}

/**
  * An agent body has an id and it carries a packet or none
  * @param id unique ID of the body which is equals to the agent id
  * @param load the body carries on a packet, eventually non
  */
case class Body(id: Int, var load: Option[Packet] = None) extends Entity{
  override def toString: String = s"B$id($load)"

  /**
    * Returns the cost of the current load, i.e
    * the size of the packet eventually 0
    */
  def charge() : Int = load match {
    case Some(packet) => packet.size
    case None => 0
  }

  /**
    * Carry a packet (setter of load)
    */
  def take(packet: Packet) : Unit = {
    load = Some(packet)
  }

  /**
    * Unload the packet it carries (setter of load)
    */
  def unload() : Unit = {
    load = None
  }

  /**
    * Two body are equals if they have the same id
    */
  override def equals(that: Any): Boolean = {
    that match {
      case that: Body => that.id == this.id
      case _ => false
    }
  }
}

/**
  * A packet has an id and a size, eventually a color if it is targeted by an agent
  */
case class Packet(id: Int, size: Int, var color: Color = Brown) extends Entity{
  override def toString: String = s"P$id($size)"
  /**
    * Two packets are equals if they have the same id
    */
  override def equals(that: Any): Boolean = {
    that match {
      case that: Packet => that.id == this.id
      case _ => false
    }
  }
}