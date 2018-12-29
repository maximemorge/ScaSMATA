// Copyright (C) Maxime MORGE 2018
package org.scasmata.environment

import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  * A representation of the environment which contains 1 destination, n agents and m packets
  * @param height of the environment
  * @param width of the environment
  * @param minSizePackets minimal size of the packets (1 by default)
  * @param maxSizePackets maximal size of the packets (2 by default)
  */
class Environment(val height: Int, val width: Int, val n: Int = 1, val m: Int = 1, val minSizePackets: Int = 1, val maxSizePackets: Int = 2) {
  val debug = false

  //Create the grid and the maps of packets/bodies
  private val grid = Array.ofDim[Cell](height, width)
  for (i <- 0 until height; j <- 0 until width)
    grid(i)(j) = new Cell(i,j)
  var packets = Map[Int,Packet]()
  var bodies  = Map[Int,Body]()
  var coalitions = Map[Seq[Int],Body]()
  // Number of packets which are putted down
  private var nbCollectedPackets = 0

  /**
    * Returns true if all the packets are collected
    */
  def isClean() : Boolean = nbCollectedPackets == m

  //Initiate a random environment
  init()

  /**
    * Clear the environment
    */
  def reset() : Unit = {
    for (i <- 0 until height; j <- 0 until width)
      grid(i)(j).setContent(None)
    packets = Map[Int,Packet]()
    bodies  = Map[Int,Body]()
    coalitions = Map[Seq[Int],Body]()
    nbCollectedPackets = 0
  }
  /**
    * Initiate a random environment such as each entity has no neighbor,
    * such that the destination and the packets are available for each agent
    */
  def init() : Unit = {
    val random = new Random
    var idBody = 0
    var idPacket = 0
    var coordinates = ListBuffer[(Int,Int)]() // List of coordinates of free cells
    for (j <- 0 until width; i <- 0 until height) coordinates += ((i,j))
    for (k <- 0 until n){// Add n agent bodies
      idBody += 1
      val (i, j) = coordinates.remove(random.nextInt(coordinates.length))
      coordinates --= Seq((i,j-1),(i,j+1),(i-1,j),(i+1,j),(i-1,j+1),(i-1,j-1),(i+1,j-1),(i+1,j+1))
      if (coordinates.isEmpty) throw new RuntimeException("Too many bodies in the environment")
      if (debug) println(s"Add body in ($i, $j)")
      val newBody = new Body(id = idBody)// TODO Check
      bodies += (idBody -> newBody)
      grid(i)(j).setContent(Some(newBody))
    }
    for (k <- 0 until m){// Add m packets
      idPacket += 1
      val (i, j) = coordinates.remove(random.nextInt(coordinates.length))
      coordinates --= Seq((i,j-1),(i,j+1),(i-1,j),(i+1,j),(i-1,j+1),(i-1,j-1),(i+1,j-1),(i+1,j+1))
      if (coordinates.isEmpty) throw new RuntimeException("Too many packets in the environment")
      if (debug) println(s"Add packet in ($i, $j)")
      val newPacket = Packet(id = idPacket, size = minSizePackets+random.nextInt(maxSizePackets))
      packets += (idPacket -> newPacket)
      grid(i)(j).setContent(Some(newPacket))
    }
    // Add the destination
    val (i, j) = coordinates.remove(random.nextInt(coordinates.length))
    if (debug) println(s"Add destination in ($i, $j)")
    grid(i)(j).setContent(Some(Destination()))
    if (debug) println(this.toString)
  }

  /**
    * Clear and initiate a random environment
    */
  def reInit() : Unit ={
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
        s+=grid(i)(j).toString
      }
      s+="| \n"
    }
    s
  }


  /**
    * Returns true if the cell (i,j) is empty
    */
  def isEmpty(i: Int, j: Int):  Boolean =  get(i,j).isEmpty
  /* Returns the list of bodies
  def bodies() : Iterable[AgentBody] = {
    (for (j <- 0 until width; i <- 0 until height) yield {
      grid(i)(j).content match {
        case body : Some[AgentBody] => body
        case _ => None
      }
    }).toIterable.filter(_.isDefined).map(_.get)
  }*/

  /**
    * Returns true if the cell (i,j) is accessible, i.e. empty or contains a body
    */
  def isAccessible(i: Int, j: Int):  Boolean =  get(i,j).isAccessible

  /**
    * Returns the list of packets by size
    */
  def packetsOfSize(size : Int): Iterable[Packet] = packets.values.filter(_.size == size)

  /**
    * Returns the list of packets of size = 1
    */
  def lightweightPackets(): Iterable[Packet] = packets.values.filter(_.size == 1)

  /**
    * Returns the list of packets of size > 1
    */
  def heavyPackets(): Iterable[Packet] = packets.values.filter(_.size > 1)

  /**
    * Returns the coordinates of an entity
    */
  def location(entity: Entity): (Int,Int) = {
    for (j <- 0 until width; i <- 0 until height) {
      grid(i)(j).content match {
        case Some(content) if entity == content => return (i, j)
        case _ =>
      }
    }
    throw new RuntimeException(s"Entity $entity is not in the environment")
  }

  /**
    * Returns the coordinates of the location
    */
  def destinationLocation(): (Int,Int) = {
    for (j <- 0 until width; i <- 0 until height){
      grid(i)(j).content match {
        case Some(Destination())  => return (i,j)
        case _ =>
      }
    }
    throw new RuntimeException(s"There is no destination in the environment")
  }

  /**
    * Returns the neighborhood of a cell
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
    * Returns the possible directions for move from (i,j)
    */
  def emptyDirection(i : Int, j : Int) : Seq[Direction] = {
    var directions = Seq[Direction](Center)
    if (i>0 && isEmpty(i-1,j)) directions +:= North
    if (j>0 && isEmpty(i,j-1)) directions +:= West
    if (i<height-1 && isEmpty(i+1,j)) directions +:= South
    if (j<width-1 && isEmpty(i,j+1)) directions +:= East
    directions
  }

  /**
    * Returns true if a move is possible, i.e. not in a destination or a packet
    */
  def isAccessibleDirection(body : Body, d :Direction) : Boolean = {
    val (i,j) = location(body)
    d match {
      case North => i>0 && isAccessible(i-1,j)
      case West => j>0 && isAccessible(i,j-1)
      case South => i<height-1 && isAccessible(i+1,j)
      case East => j<width-1 && isAccessible(i,j+1)
      case Center => true
    }
  }

  /**
    * Updates the environment with a move
    */
  def updateMove(activeEntity: ActiveEntity, d: Direction) : Unit = {
    val (i,j) = location(activeEntity)
    val (k,l) = d match {
      case East => (i,j+1)
      case North => (i-1,j)
      case West => (i,j-1)
      case South => (i+1,j)
      case Center => (i,j)
    }
    val c1 = grid(i)(j).content
    val c2 = grid(k)(l).content
    grid(i)(j).setContent(c2)
    grid(k)(l).setContent(c1)
  }

  /**
    * Updates the environment when a body pick up the environment
    */
  def updatePickUp(entity: ActiveEntity, packet: Packet): Unit = {
    val (i,j) = location(entity)
    val (x,y) = location(packet)
    grid(x)(y).setContent(None)
    entity.take(packet)
    packets = packets.filterKeys(_ != packet.id)
    grid(i)(j).setContent(Some(entity))
  }

  /**
    * Updates the environment when a body pick up the environment
    */
  def updatePutDown(entity: ActiveEntity, packet: Packet): Unit = {
    val (i,j) = location(entity)
    entity.unload()
    nbCollectedPackets +=1
    grid(i)(j).setContent(Some(entity))
  }

  /**
    * Update the environment with a target
    */
  def updateTarget(bodyId: Int, packet: Packet): Unit = {
    if (debug) println(s"Environment is looking for packet $packet")
    val (i,j) = location(packet)
    packet.color = Color.BELONGINGS(bodyId)
    grid(i)(j).setContent(Some(packet))
  }

  /**
    * Update the environment with a target
    */
  def updateTarget(bodyIds: Seq[Int], packet: Packet): Unit = {
    if (debug) println(s"Environment is looking for packet $packet")
    val (i,j) = location(packet)
    packet.color = Color.COLLECTIVE_BELONGINGS(bodyIds)
    grid(i)(j).setContent(Some(packet))
  }


  /**
    * Returns true if the destination is closed to the body
    */
  def closedDestination(entity: ActiveEntity): Boolean = {
    val (i, j) = location(entity)
    neighborhood(i, j).exists(c => c.hasDestination)
  }

  /**
    * Returns true if the body is closed to a packet
    */
  def closedPacket(entity: Entity): Boolean = {
    val (i, j) = location(entity)
    neighborhood(i, j).exists(c => c.hasPacket)
  }

  /**
    * Returns true if the body is closed to a packet
    */
  def closedPacket(entity: ActiveEntity, packet: Packet): Boolean = {
    val (i, j) = location(entity)
    neighborhood(i, j).exists(c => c.hasPacket(packet))
  }

}

