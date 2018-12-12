// Copyright (C) Maxime MORGE 2018
package org.scasmata.environment

import scala.util.Random
import scala.collection.mutable.ListBuffer

/**
  * A representation of the environment
  * @param height of the environment
  * @param width of the environment
  */
class Environment(val height: Int, val width: Int) {
  val debug = false

  val nbCollectionPoints = 1
  val nbAgentBodies  = 1
  val nbPackets = 1

  val colorPackets = Red
  val colorCollectionPoint = Red
  val sizePacket = 1

  //Create the grid
  private val grid = Array.ofDim[Cell](height, width)
  for (i <- 0 until height; j <- 0 until width)
    grid(i)(j) = new Cell(i,j)

  //Initiate a random environment
  init()

  /**
    * Clear the environment
    */
  def reset() : Unit = {
    for (i <- 0 until height; j <- 0 until width)
      grid(i)(j).setContent(NoEntity)
  }
  /**
    * Initiate a random environment
    */
  def init() : Unit = {
    val random = new Random
    var idAgent = 0
    var idPacket = 0
    var coordinates = ListBuffer[(Int,Int)]()
    for (j <- 0 until width; i <- 0 until height) coordinates += ((i,j))
    for (k <- 0 until nbAgentBodies) {
      idAgent += 1
      val (i, j) = coordinates.remove(random.nextInt(coordinates.length))
      if (debug) println(s"Add body in ($i, $j)")
      grid(i)(j).setContent(AgentBody(id = idAgent))
    }
    for (k <- 0 until nbPackets) {
      idPacket += 1
      val (i, j) = coordinates.remove(random.nextInt(coordinates.length))
      if (debug) println(s"Add packet in ($i, $j)")
      grid(i)(j).setContent(Packet(id = idPacket, colorPackets, size = sizePacket))
    }
    for (k <- 0 until nbCollectionPoints) {
      val (i, j) = coordinates.remove(random.nextInt(coordinates.length))
      if (debug) println(s"Add collectionPoint in ($i, $j)")
      grid(i)(j).setContent(CollectionPoint(colorCollectionPoint))
    }
  }

  /**
    * Clear and initiate a random environment
    */
  def reinit() : Unit ={
    reset()
    init()
  }

  /**
    * Returns the state of the cell
    * @param i th line
    * @param j th column
    */
  def get(i: Int, j: Int): Cell = grid(i)(j)


  /**
    * Returns the list of agent bodies
    */
  def bodies() : Iterable[AgentBody] = {
    (for (j <- 0 until width; i <- 0 until height) yield {
      grid(i)(j).content match {
        case AgentBody(id) => Some(AgentBody(id))
        case _ => None
      }
    }).toIterable.filter(_.isDefined).map(_.get)
  }

}

