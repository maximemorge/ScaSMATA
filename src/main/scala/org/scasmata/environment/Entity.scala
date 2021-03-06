// Copyright (C) Maxime MORGE 2018
package org.scasmata.environment

/**
  * An entity of the environment is active or passive
  */
abstract class Entity {

  /**
    * Copy the entity
    */
  def copy(): Entity = this
}

/**
  * A passive entity is a packet or the destination
  */
class PassiveEntity() extends Entity

/**
  * Destination where to put packets
  */
class Destination() extends PassiveEntity{
  override def toString: String = "D"
}

/**
  * A packet has an id and a size, eventually a color if it is targeted by an agent
  */
class Packet(val id: Int, val weight: Int, var color: Color = Brown) extends PassiveEntity {
  override def toString: String = s"P$id($weight)"

  /**
    * Two packets are equals if they have the same id
    */
  override def equals(that: Any): Boolean = {
    that match {
      case that: Packet => that.id == this.id
      case _ => false
    }
  }
  /**
    * Copy the packet
    */
  override def copy(): Packet = new Packet(id, weight, color)
}

/**
  * An active entity can influence the environment
  * @param id of the entity which is equals to the agent id
  * @param load the entity eventually carries on a packet
  */
case class ActiveEntity(val id: Int, var load: Option[Packet] = None) extends Entity {
  val capacity : Int = 0 // size of the packet it can carry on

  override def toString: String = s"AE$id($load)"
  /**
    * Two active entities are equals if they have the same id
    */
  override def equals(that: Any): Boolean = {
    that match {
      case that: ActiveEntity => that.id == this.id
      case _ => false
    }
  }

  /**
    * Copy the entity
    */
  override def copy(): ActiveEntity = {
    val newLoad : Option[Packet]= if (load.nonEmpty) Some(load.get.copy()) else None
    ActiveEntity(id, newLoad)
  }

  /**
    * Returns the cost of the current load, i.e
    * the size of the packet eventually 0
    */
  def charge : Int = load match {
    case Some(packet) => packet.weight
    case None => 0
  }

  /**
    * Returns true if the entity is loaded
    */
  def isLoaded : Boolean = charge != 0

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
  * A body correspond to a micro-level agent
  * @param id of the body which is equals to the agent id
  * @param load the body eventually carries on a packet
  */
class Body(id: Int, load : Option[Packet] = None) extends ActiveEntity(id, load){
  override val capacity = 1 // size of the packet it can carry on

  override def toString: String = s"B$id($load)"

  /**
    * Copy the body
    */
  override def copy(): Body = {
    val newLoad : Option[Packet]= if (load.nonEmpty) Some(load.get.copy()) else None
    new Body(id, newLoad)
  }
}

/**
  * A team correspond to a macro-level agent
  * @param id of the bodies which is equals to the agent id
  * @param load the body eventually carries on a packet
  * @param bodies which consist of the team
  */
class Team(id: Int, load: Option[Packet] = None, val bodies : Set[Body])
  extends ActiveEntity(id, load){
  override val capacity : Int = bodies.size // size of the packet it can carry on
  override def toString: String = s"C$ids($load)"
  // The ids of the bodies
  val ids  : Seq[Int] = bodies.map(b => b.id).toSeq.sorted

  /**
    * Copy the team
    */
  override def copy(): Team = {
    val newLoad : Option[Packet]= if (load.nonEmpty) Some(load.get.copy()) else None
    val newBodies = for (b <- bodies) yield b.copy()
    new Team(id, newLoad, newBodies)
  }

}