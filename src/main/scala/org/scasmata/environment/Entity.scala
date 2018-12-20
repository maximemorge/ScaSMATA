// Copyright (C) Maxime MORGE 2018
package org.scasmata.environment

/**
  * Entity in the environment are bodies, packets, destination or nothing (if the cell is empty)
  */
abstract class Entity
case object NoEntity extends Entity{
  override def toString: String = s"   "
}
case class Destination()
  extends Entity{
  override def toString: String = s"D  "
}
case class AgentBody(id : Int, load: Int = 0)extends Entity{
  override def toString: String = s"B$id$load"
}
case class Packet(id: Int, size : Int) extends Entity{
  override def toString: String = s"P$id$size"
}



