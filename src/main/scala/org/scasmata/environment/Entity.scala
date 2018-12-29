// Copyright (C) Maxime MORGE 2018
package org.scasmata.environment

/**
  * Entity in the environment are active entity, packets, destination or nothing (if the cell is empty)
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

case class ActiveEntity(var load: Option[Packet] = None) extends Entity {
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
}
/**
  * An agent body has an id and it carries a packet or none
  * @param id unique ID of the body which is equals to the agent id
  * @param load the body carries on a packet, eventually none
  */
class Body(val id: Int, load : Option[Packet] = None) extends ActiveEntity(load){
  override def toString: String = s"B$id($load)"

  /**
    * Two bodies are equals if they have the same id
    */
  override def equals(that: Any): Boolean = {
    that match {
      case that: Body => that.id == this.id
      case _ => false
    }
  }
}

/**
  * A coalition has a set of ids and it carries a packet or none
  * @param ids IDs of the bodies which is equals to the agent id
  * @param load the body carries on a packet, eventually none
  */
class Coalition(val ids: Seq[Int], load: Option[Packet] = None) extends ActiveEntity{
  override def toString: String = s"C$ids($load)"
  /**
    * Two bodies are equals if they have the same ids
    */
  override def equals(that: Any): Boolean = {
    that match {
      case that: Coalition => that.ids == this.ids
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