// Copyright (C) Maxime MORGE 2018
package org.scasmata.environment

/**
  * Color for Packets and CollectionPoint
  */
object Color {
val MAPPING = Map(1->Red, 2->Green, 3->Blue)
}

sealed trait Color
case object Red extends Color{
  override def toString: String = "red"
}
case object Green extends Color{
  override def toString: String = "green"
}
case object Blue extends Color{
  override def toString: String = "blue"
}
case object Brown extends Color{
  override def toString: String = "brown"
}

