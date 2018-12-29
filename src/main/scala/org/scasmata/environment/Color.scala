// Copyright (C) Maxime MORGE 2018
package org.scasmata.environment

import org.scasmata.environment

/**
  * Color for Packets and CollectionPoint
  */
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
case object Black extends Color{
  override def toString: String = "black"
}
case object RedGreen extends Color{
  override def toString: String = "redgreen"
}
case object RedBlue extends Color{
  override def toString: String = "redblue"
}
case object GreenBlue extends Color {
  override def toString: String = "greenblue"
}
case object BlackRed extends Color{
  override def toString: String = "blackred"
}
case object BlackGreen extends Color{
  override def toString: String = "blackgreen"
}
case object BlackBlue extends Color{
  override def toString: String = "blackblue"
}
case object Brown extends Color{
  override def toString: String = "brown"
}

/**
  * Companion object
  */
object Color {
  // Mapping between agentId and color
  val BELONGINGS = Map(1->Red, 2->Green, 3->Blue, 4 -> Black)
  val COLLECTIVE_BELONGINGS = Map(
    Seq(1,2) -> RedGreen, Seq(1,3) -> RedBlue, Seq(1,4) -> BlackRed,
    Seq(2,3) -> GreenBlue, Seq(2,4) -> BlackGreen,
    Seq(3,4) -> BlackBlue
  )

  /**
    * Returns the color which mixes the color c1 and the color c2
    */
  def mix(c1: Color, c2: Color) : Color ={
    (c1,c2) match {
      case (Red,Green) => RedGreen
      case (Red,Blue) => RedBlue
      case (Red,Black) => BlackRed
      case (Green,Red) => RedGreen
      case (Green,Blue) => GreenBlue
      case (Green,Black) => BlackGreen
      case (Blue,Red) => RedBlue
      case (Blue,Green) => GreenBlue
      case (Blue,Black) => BlackBlue
      case (Black,Green) => BlackGreen
      case (Black,Blue) => BlackBlue
      case (Black,Red) => BlackRed
      case _ =>
        new RuntimeException("Undefined mix color")
        Brown
    }
  }
}
