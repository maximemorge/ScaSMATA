// Copyright (C) Maxime MORGE 2018
package org.scasmata.environment

import java.awt.Image
import javax.swing.ImageIcon

import scala.swing.{Label, Publisher}
import scala.swing.event.ValueChanged

/**
  * Class representing a cell of the environment
  * @param i the row
  * @param j the column
  */
class Cell(i: Int, j : Int)extends Publisher{
  val debug = false

  // The cell content is eventually an entity, i.e. an active entity or a packet or a destination
  var content : Option[Entity] = None

  override def toString: String = "|"+(content match {
    case Some(e) => s"$e"
    case None => " "
  }).formatted(s"%${Cell.CELL_LENGTH}s")

  /**
    * Returns the cell representation within a label with an icon
    */
  def label() : Label = {
    // path of the icon
    val path = (content match {
      case Some(_: Destination) =>
        "brownPlace"
      case Some(b : Body) =>
        "fig"+b.id.toString +
          (if (b.load.isDefined) "load" else "")
      case Some(c : Crowd) =>
        "fig"+c.ids.sorted.map(_.toString).reduce((left, right) => s"$left$right") +
          (if (c.load.isDefined) "load" else "")
      case Some(p : Packet) =>
        p.color.toString+p.size.toString
      case _ =>
        "nothing"
    }) + ".png"
    if (debug) println(s"Show image $path")
    val url = getClass.getResource(path)
    //In order to resize the image
    val image = new ImageIcon(new ImageIcon(url).getImage.getScaledInstance(100, 100, Image.SCALE_SMOOTH))
    new Label{ icon = image }
  }

  /**
    * Change the content of the cell and publish it
    */
  def setContent(entity: Option[Entity]) : Unit  = {
    content = entity
    publish(new ValueChanged(label()))
  }

  /**
    *  Returns true of the cell contains no entity
    */
  def isEmpty : Boolean = content.isEmpty

  /**
    * Returns true if the cell contains no moving entity
    */
  def isAccessible : Boolean = content match {
    case None => true
    case Some(_ : ActiveEntity) => true
    case Some(_ : PassiveEntity) => false
    case _ => throw new RuntimeException("Unexpected content of cell")
  }

  /**
    * Returns true if the cell contains an ActiveEntity
    */
  def hasActiveEntity : Boolean = content match {
    case Some(_: ActiveEntity) => true
    case _ => false
  }

  /**
    * Returns true if the cell contains a PassiveEntity
    */
  def hasPassiveEntity : Boolean = content match {
    case Some(_: PassiveEntity) => true
    case _ => false
  }

  /**
    * Returns true if the cell contains a packet
    */
  def hasPacket : Boolean = content match {
    case Some(_: Packet) => true
    case _ => false
  }

  /**
    * Returns true if the cell contains a particular packet
    */
  def hasPacket(packet: Packet) : Boolean =
    content match {
      case Some(p: Packet) if p == packet=> true
      case _ => false
    }

  /**
    * Returns true if the cell contains a destination
    */
  def hasDestination : Boolean = content match {
    case Some(_: Destination) => true
    case _ => false
  }

  /**
    * Returns true if the cell contains a body
    */
  def hasBody : Boolean = content match {
    case Some(_: Body) => true
    case _ => false
  }

  /**
    * Returns true if the cell contains a particular body
    */
  def hasBody(body: Body) : Boolean =
    content match {
      case Some(b: Body) if b == body=> true
      case _ => false
    }

  /**
    * Returns true if the cell contains a particular active entity
    */
  def hasActiveEntity(entity: ActiveEntity) : Boolean =
    content match {
      case Some(e: ActiveEntity) if e == entity => true
      case _ => false
    }
}

/**
  * Companion object for class variable
  */
object Cell{
val CELL_LENGTH = 8 // length of the string representation
}