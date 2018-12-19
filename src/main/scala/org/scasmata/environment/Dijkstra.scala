// Copyright (C) Maxime MORGE 2018
package org.scasmata.environment

/**
  * Implementation fo Dijkstra's algorithm for finding the shortest paths from a origin cell in an environment
  * Note : we assume here that any resource is available by any agent (i.e. resource and destination are reachable)
  * TODO : check it in the environment generation
  * @param e the environment
  * @param oi line of the origine
  * @param oj column of the origine
  */
class Dijkstra(e : Environment, oi : Int, oj : Int) {
  val debug = true

  //Create and initiate the matrix of distances
  private val distance = Array.ofDim[Int](e.height, e.width)
  for (i <- 0 until e.height; j <- 0 until e.width){ // all the cells are far away
    distance(i)(j) = Int.MaxValue
  }
  distance(oi)(oj)=0// excepted the origin cell

  //Create the list of cells which are unexplored, initially all the cells
  var unexplored = Seq[(Int,Int)]()
  for (i <- 0 until e.height; j <- 0 until e.width) {
   unexplored :+= (i,j)
  }

  //Create the predecessor relationship
  var predecessor = Map[(Int,Int),(Int,Int)]()

  /**
    * find a unexplored cell with a minimal distance from the source and (-1,-1) if none
    */
  def findMinDistanceUnexploredCell() : (Int,Int) = {
    var min = Int.MaxValue
    var (x,y) = (-1,-1)
    unexplored.foreach{ case (i,j) =>
      if (distance(i)(j) < min){
        min = distance(i)(j)
        x = i
        y = j
      }
    }
    (x,y)
  }

  /**
    * Update the distance and the predecessor relation between a source and a destination
    */
  def updateDistancePredecessor(source: (Int,Int), destination: (Int,Int)) = {
    val (sx,sy) = source
    val (dx,dy) = destination
    if (distance(dx)(dy) > distance(sx)(sy)+1 && e.get(dx,dy).isEmpty){ // If we find a shortest path
      // Update distance (dx,dy) and the predecessor
      distance(dx)(dy) = distance(sx)(sy)+1
      predecessor += (destination -> source)
    }
  }

  /**
    * Update the distance/precedecessor of a particular reachable target according to the reachable neighbors
    */
  def updateTarget(target: (Int,Int)) ={
    val (i,j) = target
    // Computes the reachable neighbors
    var neighbors = Seq[(Int,Int)]()
    if (i>0 && distance(i-1)(j)!=Int.MaxValue) neighbors :+= (i-1,j)
    if (j>0 && distance(i)(j-1)!=Int.MaxValue ) neighbors :+= (i,j-1)
    if (j>0 && i>0 && distance(i-1)(j-1)!=Int.MaxValue ) neighbors :+= (i-1,j-1)
    if (i< e.height-1 && distance(i+1)(j)!=Int.MaxValue) neighbors :+=  (i+1,j)
    if (j< e.width-1 && distance(i)(j+1)!=Int.MaxValue) neighbors :+=  (i,j+1)
    if (i< e.height-1 && j< e.width-1 && distance(i+1)(j+1)!=Int.MaxValue) neighbors :+=  (i+1,j+1)
    //Update the distance/precedecessor
    neighbors.foreach { case (ni, nj) =>
      if (distance(i)(j) > distance(ni)(nj) +1){
        distance(i)(j) = distance(ni)(nj) +1
        predecessor += ( (i,j) -> (ni,nj))
      }
    }
  }

  /**
    * Run Dijkstra algorithm
    */
  def run() =  {
    // While there is a unexplored cell which is reachable (which is empty or contains the agent)
    while(!unexplored.isEmpty && findMinDistanceUnexploredCell() != (-1,-1)){
      // find the closest cell
      val (i,j) = findMinDistanceUnexploredCell()
      unexplored = unexplored.filterNot(_ ==(i,j))
      if (e.get(i,j).isEmpty || (i,j) == (oi,oj)){// If the cell is reachable (which is empty or contains the agent)
        // update the distance and the predecessor relation between the cell and the neighbor (by moving)
        if (i>0) updateDistancePredecessor((i,j), (i-1,j))
        if (j>0) updateDistancePredecessor((i,j), (i,j-1))
        if (i< e.height-1) updateDistancePredecessor((i,j),(i+1,j))
        if (j< e.width-1) updateDistancePredecessor((i,j),(i,j+1))
      }
    }
    if (debug) printDistances()
  }

  /**
    * Returns the next cell in order to go to (dx,dy) through the shortest shortest path
    */
  def nextCellTo(dx: Int, dy: Int) : (Int,Int) = {
    var currentCell = (dx,dy)
    var previousCell = (-1,-1)
    var stop = false
      while (! stop){
        // find the previous cell
        previousCell = predecessor(currentCell)
        if (previousCell==(oi,oj)){// until the origin cell
          stop = true
        } else {
          currentCell = previousCell
        }

      }
    currentCell
  }

  /**
    * Returns the next direction in order to go to (dx,dy) with the shortest path
    */
  def nextDirectionTo(dx: Int, dy: Int) : Direction = {
    updateTarget(dx,dy)
    val (i,j) = nextCellTo(dx,dy)
    if (debug) println("The direction from the next step from ($oi,$oj) to ($dx,$dy) is ($i,$j)")
    if (i>oi) return South
    if (i<oi) return North
    if (j>oj) return East
    West
  }

  /**
    * Print the distance matrix
    */
  def printDistances() ={
    for (i <- 0 until e.height) {
      for (j <- 0 until e.width) {
        val sd = if (distance(i)(j) == Int.MaxValue) "+∞" else "%02d".format(distance(i)(j))
        print(s"| $sd ")
      }
      println("|")
    }
    println("")
  }

}
