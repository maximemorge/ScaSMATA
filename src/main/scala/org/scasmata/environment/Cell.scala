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

  var content : Entity = NoEntity

  /**
    * Returns the cell representation within a label with an icon
    */
  def label() : Label = {
    val path = (content match {
      case CollectionPoint(color) =>
        color+"Place"
      case AgentBody(id) =>
        "fig"+id.toString
      case Packet(_,color,size) =>
        color+size.toString
      case NoEntity =>
        "nothing"
    }) + ".png"
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
}