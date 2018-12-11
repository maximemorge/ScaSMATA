// Copyright (C) Maxime MORGE 2018
package org.scasmata.environment

import java.awt.Image
import javax.swing.ImageIcon
import scala.swing.{Label, Publisher}
import scala.swing.event.ValueChanged

/**
  * A representation of the environment
  * @param height of the environment
  * @param width of the environment
  * @param maxNumberOfAgents maximum number of agents
  * @param maxNumberOfPackets maximum number of packets
  */
class Environment(val height: Int, val width: Int, maxNumberOfAgents: Int, maxNumberOfPackets : Int) {

  val maxNumberOfCollectionPoints =1
  val numberOfStates = 4

  private val grid = Array.ofDim[Cell](height, width)
  for (i <- 0 until height; j <- 0 until width)
    grid(i)(j) = new Cell(i,j)
  init()

  /**
    * Initiate a random environment
    */
  def init() : Unit = {
    var idAgent = 0
    var idPacket = 0
    var nbCollectionPoints = 0
    for (j <- 0 until width;i <- 0 until height) {
       (math.random * numberOfStates).toInt match {
        case 0 if nbCollectionPoints < maxNumberOfCollectionPoints =>
          nbCollectionPoints += 1
          grid(i)(j).setContent(CollectionPoint(Red))
        case 1 if idAgent < maxNumberOfAgents =>
          idAgent += 1
          grid(i)(j).setContent(AgentBody(id = idAgent))
        case 2 if idPacket < maxNumberOfPackets=>
          idPacket += 1
          grid(i)(j).setContent(Packet(id = idPacket, Red, size = 1))
        case _ =>
          grid(i)(j).setContent(NoEntity)
      }
    }
  }

  /**
    * Returns the state of the cell
    * @param i th line
    * @param j th column
    */
  def get(i: Int, j: Int): Cell = grid(i)(j)
}

