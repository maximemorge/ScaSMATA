// Copyright (C) Maxime MORGE 2018
package org.scasmata.environment

import java.awt.Image
import javax.swing.ImageIcon
import scala.swing.{Label, Publisher}
import scala.swing.event.ValueChanged

/**
  * Class representing a cell of the environment which publish the modification
  * @param i the row
  * @param j the column
  */
class Cell(i: Int, j : Int) extends Publisher{
  val debug = false
  var content : Entity = NoEntity

  /**
    * Returns the cell representation within a label with an icon
    */
  def label() : Label = {
    val path = (content match {
      case Destination(color) =>
        color+"Place"
      case AgentBody(id,load) =>
        if (load == 0) "fig"+id.toString
        else  "fig"+id.toString+"red"
      case Packet(_,color,size) =>
        color+size.toString
      case NoEntity =>
        "nothing"
    }) + ".png"
    if (debug) println(path)
    val url = getClass.getResource(path)
    //In order to resize the image
    val image = new ImageIcon(new ImageIcon(url).getImage.getScaledInstance(100, 100, Image.SCALE_SMOOTH))
    new Label{ icon = image }
  }

  /**
    * Change the content of the cell and publish it
    * @param entity inside the cell
    */
  def setContent(entity: Entity) : Unit  = {
    if (entity != content){
      content = entity
      publish(new ValueChanged(label()))
    }
  }

  /**
    * Returns true of the cell contains no entity
    */
  def isEmpty() : Boolean = content == NoEntity

  /**
    * Returns true if the cell contains the body with a particular id
    */
  def hasBody(id : Int) : Boolean = content.isInstanceOf[AgentBody] && content.asInstanceOf[AgentBody].id == id

  /**
    * Returns true if the cell contains a packet
    */
  def hasPacket() : Boolean = content.isInstanceOf[Packet]

  /**
    * Returns true if the cell contains a destination
    */
  def hasDestination() : Boolean = content.isInstanceOf[Destination]
}