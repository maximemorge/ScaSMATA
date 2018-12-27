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
  var bodies  = Map[Int,AgentBody]()
  // Number of packets which are still available to be picked up in the environment
  def nbScatteredPackets : Int= packets.keys.size


  //Initiate a random environment
  init()

  /**
    * Clear the environment
    */
  def reset() : Unit = {
    for (i <- 0 until height; j <- 0 until width)
      grid(i)(j).setContent(None)
    packets = Map[Int,Packet]()
    bodies  = Map[Int,AgentBody]()
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
      coordinates --= Seq((i,j-1),(i,j+1),(i-1,j+1))
      if (debug) println(s"Add body in ($i, $j)")
      val newBody = AgentBody(id = idBody)
      bodies += (idBody -> newBody)
      grid(i)(j).setContent(Some(newBody))
    }
    for (k <- 0 until m){// Add m packets
      idPacket += 1
      val (i, j) = coordinates.remove(random.nextInt(coordinates.length))
      coordinates --= Seq((i,j-1),(i,j+1),(i-1,j+1))
      if (debug) println(s"Add packet in ($i, $j)")
      val newPacket = Packet(id = idPacket, size = minSizePackets+random.nextInt(maxSizePackets))
      packets += (idPacket -> newPacket)
      grid(i)(j).setContent(Some(newPacket))
    }
    // Add the destination
    val (i, j) = coordinates.remove(random.nextInt(coordinates.length))
    coordinates --= Seq((i,j-1),(i,j+1),(i-1,j+1))
    if (debug) println(s"Add destination in ($i, $j)")
    grid(i)(j).setContent(Some(Destination()))
    if (debug) println(this.toString)
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

  /*
    * Returns the list of bodies
  def bodies() : Iterable[AgentBody] = {
    (for (j <- 0 until width; i <- 0 until height) yield {
      grid(i)(j).content match {
        case body : Some[AgentBody] => body
        case _ => None
      }
    }).toIterable.filter(_.isDefined).map(_.get)
  }*/

  /*
    * Returns the body with a particular id
  def body(id: Int) : AgentBody = {
    for (j <- 0 until width; i <- 0 until height){
      grid(i)(j).content match {
        case body : AgentBody if body.id == id => return body
        case _ => None
      }
    }
    new RuntimeException(s"Body $id is not in the environment")
    AgentBody(0)
  }*/

  /**
    * Returns the list of packets by size
    */
  def packetsOfSize(size : Int) : Iterable[Packet] = packets.values.filter(_.size == size)
  /*def packets(size : Int) : Iterable[Packet] = {
    (for (j <- 0 until width; i <- 0 until height) yield {
      grid(i)(j).content match {
        case packet : Some[Packet] if packet.size == size => packet
        case _ => None
      }
    }).toIterable.filter(_.isDefined).map(_.get)
  }*/

  /**
    * Returns the coordinates of the body
    */
  def location(body: AgentBody): (Int,Int) = {
    for (j <- 0 until width; i <- 0 until height) {
      grid(i)(j).content match {
        case Some(content) if body == content => return (i, j)
        case _ =>
      }
    }
    new RuntimeException(s"Body $body is not in the environment")
    (-1,-1)
  }


  /**
    * Returns the coordinates of the packet
    */
  def location(packet: Packet): (Int,Int) = {
    for (j <- 0 until width; i <- 0 until height){
      grid(i)(j).content match {
        case Some(content) if packet == content => return (i,j)
        case _ =>
      }
    }
    throw new RuntimeException(s"Packet $packet is not in the environment")
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
  def isPossibleDirection(body : AgentBody, d :Direction) : Boolean = {
    val (i,j) = location(body)
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
  def updateMove(body: AgentBody, d: Direction) : Unit = {
    val (i,j) = location(body)
    if (!isPossibleDirection(body, d))
      new RuntimeException(s"Move to $d from ($i,$j) is impossible")
    val entity = grid(i)(j).content
    grid(i)(j).setContent(None)
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
  def updatePickUp(body: AgentBody, packet: Packet): Unit = {
    val (i,j) = location(body)
    val (x,y) = location(packet)
    grid(x)(y).setContent(None)
    body.take(packet)
    packets = packets.filterKeys(_ != packet.id)
    grid(i)(j).setContent(Some(body))
  }

  /**
    * Updates the environment when a body pick up the environment
    */
  def updatePutDown(body: AgentBody, packet: Packet): Unit = {
    val (i,j) = location(body)
    body.unload()
    grid(i)(j).setContent(Some(body))
  }

  /**
    * Update the environment with a target
    */
  def updateTarget(bodyId: Int, packet: Packet): Unit = {
    if (debug) println(s"Environment is looking for packet $packet")
    val (i,j) = location(packet)
    packet.color = Color.MAPPING(bodyId)
    grid(i)(j).setContent(Some(packet))
  }

  /**
    * Returns true if the destination is closed to the body
    */
  def closedDestination(body: AgentBody): Boolean = {
    val (i, j) = location(body)
    neighborhood(i, j).exists(c => c.hasDestination)
  }

  /**
    * Returns true if the body is closed to a packet
    */
  def closedPacket(body: AgentBody): Boolean = {
    val (i, j) = location(body)
    neighborhood(i, j).exists(c => c.hasPacket)
  }

  /**
    * Returns true if the body is closed to a packet
    */
  def closedPacket(body: AgentBody, packet: Packet): Boolean = {
    val (i, j) = location(body)
    neighborhood(i, j).exists(c => c.hasPacket(packet))
  }

}

