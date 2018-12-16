// Copyright (C) Maxime MORGE 2018
package org.scasmata.environment

import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  * A representation of the environment
  * @param height of the environment
  * @param width of the environment
  */
class Environment(val height: Int, val width: Int,
                  val nbDestionations : Int = 1, val nbAgentBodies  : Int = 1,
                  val nbPackets : Int = 1, val maxSizePackets : Int= 1){
  val debug = false

  val colorPackets = Red
  val colorDestination = Red
  var nbScatteredPackets = nbPackets

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
    var idBody = 0
    var idPacket = 0
    var coordinates = ListBuffer[(Int,Int)]()
    for (j <- 0 until width; i <- 0 until height) coordinates += ((i,j))
    for (k <- 0 until nbAgentBodies) {
      idBody += 1
      val (i, j) = coordinates.remove(random.nextInt(coordinates.length))
      if (debug) println(s"Add body in ($i, $j)")
      grid(i)(j).setContent(AgentBody(id = idBody))
    }
    for (k <- 0 until nbPackets) {
      idPacket += 1
      val (i, j) = coordinates.remove(random.nextInt(coordinates.length))
      if (debug) println(s"Add packet in ($i, $j)")
      grid(i)(j).setContent(Packet(id = idPacket, colorPackets, size = 1+random.nextInt(maxSizePackets)))
    }
    for (k <- 0 until nbDestionations) {
      val (i, j) = coordinates.remove(random.nextInt(coordinates.length))
      if (debug) println(s"Add collectionPoint in ($i, $j)")
      grid(i)(j).setContent(Destination(colorDestination))
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
    * Returns the cell (i,j)
    */
  def get(i: Int, j: Int): Cell = grid(i)(j)


  /**
    * Returns a string representation of the environment
    */
  override def toString: String = {
    var s =""
    for (i <- 0 until height){
      for(j <- 0 until width){
        s+=get(i,j)+toString
      }
      s+"\n"
    }
    s
  }

  /**
    * Returns true if the cell (i,j) is empty
    */
  def isEmpty(i: Int, j: Int):  Boolean =  get(i,j).isEmpty

  /**
    * Returns the list of bodyIds
    */
  def bodyIds() : Iterable[Int] = {
    (for (j <- 0 until width; i <- 0 until height) yield {
      grid(i)(j).content match {
        case AgentBody(id,_) => Some(id)
        case _ => None
      }
    }).toIterable.filter(_.isDefined).map(_.get)
  }

  /**
    * Returns the coordinates of the body
    */
  def bodyLocation(bodyId: Int): (Int,Int) = {
    for (j <- 0 until width; i <- 0 until height) {
      grid(i)(j).content match {
        case AgentBody(id, _) if bodyId == id => return (i, j)
        case _ =>
      }
    }
    new RuntimeException(s"Body $bodyId is not in the environment")
    (-1,-1)
  }

  /**
    * Returns the coordinates of the packet
    */
  def packetLocation(packetId: Int): (Int,Int) = {
    for (j <- 0 until width; i <- 0 until height){
      grid(i)(j).content match {
        case Packet(id,_,_) if packetId == id => return (i,j)
        case _ =>
      }
    }
    new RuntimeException(s"Packet $packetId is not in the environment")
    (-1,-1)
  }

  /**
    * Returns the neigborhood of a cell
    */
  def neighborhood(i : Int, j : Int) : Seq[Cell] = {
    var n = Seq[Cell]()
    if (i>0) n :+=  get(i-1,j)
    if (j>0) n :+= get(i,j-1)
    if (i>0 && j>0) n :+= get(i-1,j-1)
    if (i< height-1) n :+=  get(i+1,j)
    if (j< width-1) n :+=  get(i,j+1)
    if (j< width-1 && i< height-1) n :+=  get(i+1,j+1)
    n
  }

  /**
    * Returns the possible directions for an entity in (i,j)
    */
  def possibleDirections(i : Int, j : Int) : Seq[Direction] = {
    var directions = Seq[Direction](Center)
    if (i>0 && isEmpty(i-1,j)) directions +:= North
    if (j>0 && isEmpty(i,j-1)) directions +:= West
    if (i<height-1 && isEmpty(i+1,j)) directions +:= South
    if (j<width-1 && isEmpty(i,j+1)) directions +:= East
    directions
  }

  /**
    * Returns true if a move is possible
    */
  def isPossibleDirection(bodyId : Int, d :Direction) : Boolean = {
    val (i,j) = bodyLocation(bodyId)
    d match {
      case North => i>0 && isEmpty(i-1,j)
      case West => j>0 && isEmpty(i,j-1)
      case South => i<height-1 && isEmpty(i+1,j)
      case East => j<width-1 && isEmpty(i,j+1)
      case Center => true
    }
  }

  /**
    * Updates the environment with a move
    */
  def updateMove(bodyId: Int, d: Direction) = {
    val (i,j) = bodyLocation(bodyId)
    if (!isPossibleDirection(bodyId, d))
      new RuntimeException(s"Move to $d from ($i,$j) is impossible")
    val entity = grid(i)(j).content
    grid(i)(j).setContent(NoEntity)
    val (k,l) = d match {
      case East => (i,j+1)
      case North => (i-1,j)
      case West => (i,j-1)
      case South => (i+1,j)
      case Center => (i,j)
    }
    grid(k)(l).setContent(entity)
  }

  /**
    * Updates the environment when a body pick up the environment
    */
  def updatePickUp(bodyId : Int, packetId: Int) = {
    val (i,j) = bodyLocation(bodyId)
    val (x,y) = packetLocation(packetId)
    grid(x)(y).setContent(NoEntity)
    grid(i)(j).setContent(AgentBody(bodyId, packetId))
  }

  /**
    * Updates the environment when a body pick up the environment
    */
  def updatePutDown(bodyId : Int, packetId: Int) = {
    val (i,j) = bodyLocation(bodyId)
    nbScatteredPackets -= 1
    grid(i)(j).setContent(AgentBody(bodyId))
  }


  /**
    * Returns true if a packet is closed to the body
    */
  def closedPacket(bodyId: Int) : Boolean = {
    val (i, j) = bodyLocation(bodyId)
    neighborhood(i, j).exists(c => c.hasPacket)
  }

  /**
    * Returns the list of packetIds closed to the bodyId
    */
  def closedPackets(bodyId: Int) : Seq[Int] = {
    var l = Seq[Int]()
    val (i, j) = bodyLocation(bodyId)
    neighborhood(i, j).foreach { c =>
      if (c.hasPacket)  l :+= c.content.asInstanceOf[Packet].id
    }
    l
  }

  /**
    * Returns the list of destinations closed to the bodyId
    */
  def closedDestinations(bodyId: Int, color: Color) : Seq[Destination] = {
    var l = Seq[Destination]()
    val (i, j) = bodyLocation(bodyId)
    neighborhood(i, j).foreach { c =>
      if (c.content.isInstanceOf[Destination] && c.content.asInstanceOf[Destination].color == color)
        l :+= c.content.asInstanceOf[Destination]
    }
    l
  }

  /**
    * Returns the load of a bodyId
    */
  def load(bodyId : Int) = {
    val (i,j) = bodyLocation(bodyId)
    grid(i)(j).content.asInstanceOf[AgentBody].load
  }

}

