// Copyright (C) Maxime MORGE 2018
package org.scasmata.environment

import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  * A representation of the environment which contains 1 destination, n agents and m packets
  * @param height of the environment
  * @param width of the environment
  * @param n number of bodies
  * @param m number of packets
  * @param minSizePackets minimal size of the packets (1 by default)
  * @param maxSizePackets maximal size of the packets (2 by default)
  */
class Environment(val height: Int, val width: Int, val n: Int = 1, val m: Int = 1, val minSizePackets: Int = 1, val maxSizePackets: Int = 2) {
  val debug = false

  //Create the grid and the maps of packets/bodies/teams
  private val grid = Array.ofDim[Cell](height, width)
  for (i <- 0 until height; j <- 0 until width)
    grid(i)(j) = new Cell(i,j)
  var packets : Map[Int,Packet] = Map[Int,Packet]()
  var bodies : Map[Int,Body] = Map[Int,Body]()
  var teams : Map[Int,Team]= Map[Int,Team]()

  // Number of packets which are collected
  private var nbCollectedPackets = 0
  // Id of the next team
  private var nexTeamId = n + 1

  val initialState = new EnvironmentState(height, width, packets, bodies, teams)

  /**
    * Returns the map of active entities
    */
  def activeEntities : Map[Int,ActiveEntity] = bodies ++ teams

  /**
    * Return true if an active entity is a team
    */
  def isTeam(id : Int) : Boolean = id > n


  // Number of active entities
  def nbActiveEntities : Int = bodies.size + teams.size

  /**
    * Return true if all the packets are collected
    */
  def isClean : Boolean = nbCollectedPackets == m

  /**
    * Initiate a random environment such as each entity has no neighbor, i.e.
    * the destination and the packets are available for each agent
    */
  def init() : Unit = {
    val random = new Random
    var idBody = 0
    var idPacket = 0
    var freeCells = ListBuffer[(Int,Int)]()
    for (j <- 0 until width; i <- 0 until height) freeCells += ((i,j))
    for (k <- 0 until n){// Add n agent bodies
      idBody += 1
      val (i, j) = freeCells.remove(random.nextInt(freeCells.length))
      freeCells --= Seq((i,j-1),(i,j+1),(i-1,j),(i+1,j),(i-1,j+1),(i-1,j-1),(i+1,j-1),(i+1,j+1))
      if (freeCells.isEmpty) throw new RuntimeException("Too many bodies in the environment")
      if (debug) println(s"Add body in ($i, $j)")
      val newBody = new Body(id = idBody)
      bodies += (idBody -> newBody)
      grid(i)(j).setContent(Some(newBody))
    }
    for (k <- 0 until m){// Add m packets
      idPacket += 1
      val (i, j) = freeCells.remove(random.nextInt(freeCells.length))
      freeCells --= Seq((i,j-1),(i,j+1),(i-1,j),(i+1,j),(i-1,j+1),(i-1,j-1),(i+1,j-1),(i+1,j+1))
      if (freeCells.isEmpty) throw new RuntimeException("Too many packets in the environment")
      if (debug) println(s"Add packet in ($i, $j)")
      val newPacket = new Packet(id = idPacket, weight = minSizePackets+random.nextInt(maxSizePackets-minSizePackets+1))
      packets += (idPacket -> newPacket)
      grid(i)(j).setContent(Some(newPacket))
    }
    // Add the destination
    val (i, j) = freeCells.remove(random.nextInt(freeCells.length))
    if (debug) println(s"Add destination in ($i, $j)")
    grid(i)(j).setContent(Some(new Destination()))
    if (debug) println(this.toString)
    initialState.save(grid, packets, bodies, teams)
  }

  /**
    * Clear the environment
    */
  def clear() : Unit = {
    for (i <- 0 until height; j <- 0 until width)
      grid(i)(j).setContent(None)
    packets = Map[Int,Packet]()
    bodies  = Map[Int,Body]()
    teams = Map[Int,Team]()
    nbCollectedPackets = 0
  }

  /**
    * Reset to the initial state
    */
  def reset() : Unit = {
    packets = initialState.packets
    bodies = initialState.bodies
    teams = initialState.teams
    nbCollectedPackets = 0
    for (i <- 0 until height; j <- 0 until width)
      grid(i)(j).setContent(initialState.grid(i)(j).content)
  }

  /**
    * Clear and initiate a random environment
    */
  def reGenerate() : Unit ={
    clear()
    init()
  }

  /**
    * Returns the cell (i,j)
    */
  def get(i: Int, j: Int): Cell = grid(i)(j)

  /**
    * Returns the active entity according to the id
    */
  def getActiveEntity(id : Int) : ActiveEntity = {
    if (id <= n) return bodies(id)
    teams(id)
  }

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

  /**
    * Returns true if the cell (i,j) is accessible, i.e. empty or contains a body
    */
  def isAccessible(i: Int, j: Int):  Boolean =  get(i,j).isAccessible

  /**
    * Returns the list of packets of size = 1
    */
  def lightPackets(): Iterable[Packet] = packets.values.filter(_.weight == 1)

  /**
    * Returns the list of packets of size > 1
    */
  def heavyPackets(): Iterable[Packet] = packets.values.filter(_.weight > 1)

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
        case Some(d : Destination)  => return (i,j)
        case _ =>
      }
    }
    throw new RuntimeException(s"There is no destination in the environment")
  }

  /**
    * Returns the neighborhood of a cell
    */
  def neighborhood(i : Int, j : Int) : Seq[Cell] = {
    var neighbor = Seq[Cell]()
    if (i>0) neighbor :+=  get(i-1,j)
    if (j>0) neighbor :+= get(i,j-1)
    if (i< height-1) neighbor :+=  get(i+1,j)
    if (j< width-1) neighbor :+=  get(i,j+1)
    neighbor
  }

  /**
    * Returns the possible directions for move from (i,j)
    */
  def accessibleDirections(i : Int, j : Int) : Seq[Direction] = {
    var directions = Seq[Direction](Center)
    if (i>0 && isAccessible(i-1,j)) directions +:= North
    if (j>0 && isAccessible(i,j-1)) directions +:= West
    if (i<height-1 && isAccessible(i+1,j)) directions +:= South
    if (j<width-1 && isAccessible(i,j+1)) directions +:= East
    directions
  }


  /**
    * Returns true if a move is possible, i.e. not in a passive entity
    */
  def isAccessibleDirection(entity: ActiveEntity, d :Direction) : Boolean = {
    val (i,j) = location(entity)
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
    * Updates the environment when a body pick up a packet
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
    * Updates the environment when a body put a packet down
    */
  def updatePutDown(entity: ActiveEntity, packet: Packet): Unit = {
    val (i,j) = location(entity)
    entity.unload()
    nbCollectedPackets +=1
    if (debug) println(s"NbCollectedPackets $nbCollectedPackets")
    grid(i)(j).setContent(Some(entity))
  }

  /**
    * Updates the environment when two entities merge
    */
  def updateMerge(entity1: ActiveEntity, entity2: ActiveEntity): Team = {
    bodies = bodies.filterKeys(id => id != entity1.id && id != entity2.id)
    teams = teams.filterKeys(id => id != entity1.id && id != entity2.id)
    val set : Set[Body]= (entity1 match {
      case b : Body =>
        Set(b)
      case c : Team =>
        c.bodies
      case _ =>
        throw new RuntimeException("An active entity is expected for merge")
    }) | (entity2 match {
      case b : Body =>
        Set(b)
      case c : Team =>
        c.bodies
      case _ =>
        throw new RuntimeException("An active entity is expected for merge")
    })
    val crowd = new Team(nexTeamId, None, set)
    nexTeamId += 1
    teams = teams + (crowd.id -> crowd)
    val (i,j) = location(entity1)
    val (k,l) = location(entity2)
    grid(k)(l).setContent(None)
    grid(i)(j).setContent(Some(crowd))
    if (debug) println(s"Nb activeEntitties $nbActiveEntities")
    crowd
  }

  /**
    * Return true if the team can split
    */
  def canSplit(team: Team): Boolean = {
    val newBodies = team.bodies
    val (i,j) = location(team)
    var possiblePlaces = neighborhood(i,j).filter(_.isEmpty)
    if (possiblePlaces.length < newBodies.size) return false
    true
  }

  /**
    * Update the environment when a team splits and return the set of bodies eventually none
    */
  def updateSplit(team: Team): Set[Body] = {
    val newBodies = team.bodies
    val (i,j) = location(team)
    var possiblePlaces = neighborhood(i,j).filter(_.isEmpty)
    if (! canSplit(team)) return Set()
    teams = teams.filterKeys(_ != team.id)
    grid(i)(j).setContent(None)
    newBodies.foreach{ body =>
      bodies = bodies + (body.id -> body)
      possiblePlaces.head.setContent(Some(body))
      possiblePlaces = possiblePlaces.tail
    }
    newBodies
  }

  /**
    * Update the environment with a target
    */
  def updateTarget(id: Int, packet: Packet): Unit = {
    if (debug) println(s"Environment is looking for packet $packet")
    val (i,j) = location(packet)
    packet.color = getActiveEntity(id) match {
      case b : Body =>
         Color.BELONGINGS(b.id)
      case c : Team =>
        Color.COLLECTIVE_BELONGINGS(c.ids)
      case _ =>
        throw new RuntimeException("An active entity is expected for having a target")
    }
    grid(i)(j).setContent(Some(packet))
  }

  /**
    * Returns true if the destination is closed to the entity
    */
  def closedDestination(entity: ActiveEntity): Boolean = {
    val (i, j) = location(entity)
    neighborhood(i, j).exists(c => c.hasDestination)
  }

  /**
    * Returns true if the entity is closed to a packet
    */
  def closedPacket(entity: Entity): Boolean = {
    val (i, j) = location(entity)
    neighborhood(i, j).exists(c => c.hasPacket)
  }

  /**
    * Returns true if the entity is closed to a packet
    */
  def closedPacket(entity: ActiveEntity, packet: Packet): Boolean = {
    val (i, j) = location(entity)
    neighborhood(i, j).exists(c => c.hasPacket(packet))
  }

  /**
    * Returns true if the entity is closed to a packet
    */
  def closedActiveEntity(entity1: ActiveEntity, entity2: ActiveEntity): Boolean = {
    val (i, j) = location(entity1)
    neighborhood(i, j).exists(c => c.hasActiveEntity(entity2))
  }
}

