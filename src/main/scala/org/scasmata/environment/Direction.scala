// Copyright (C) Maxime MORGE 2018
package org.scasmata.environment

/**
  * Direction for the moves of entities
  */
sealed trait Direction
case object North extends Direction { override def toString: String = "North"}
case object South extends Direction { override def toString: String = "South" }
case object East extends Direction { override def toString: String = "East" }
case object West extends Direction { override def toString: String = "West" }

