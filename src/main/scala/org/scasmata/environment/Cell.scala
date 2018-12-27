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
  var content : Option[Entity] = None

  override def toString: String = content match {
    case Some(e) => s"|$e"
    case None => "|"+"".formatted(s"%${Entity.size}s")
  }

  /**
    * Returns the cell representation within a label with an icon
    */
  def label() : Label = {
    val path = (content match {
      case Some(Destination()) =>
        "brownPlace"
      case Some(AgentBody(id,load)) =>
        if (load.isEmpty) "fig"+id.toString
        else  "fig"+id.toString+"load"
      case Some(Packet(_,size,color)) =>
        color.toString+size.toString
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

}