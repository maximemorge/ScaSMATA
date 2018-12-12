// Copyright (C) Maxime MORGE 2018
package org.scasmata.environment

/**
  * Entity in the environment are bodies, packets, collection point or nothing (if the cell is empty)
  */
abstract class Entity
case object NoEntity
  extends Entity
case class CollectionPoint(color: Color)
  extends Entity
case class AgentBody(id : Int)
  extends Entity
case class Packet(id: Int, color: Color, size : Int)
  extends Entity



