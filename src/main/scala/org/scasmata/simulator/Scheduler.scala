// Copyright (C) Maxime MORGE, Florian LECOINTE, Quentin BRIAND  2019
package org.scasmata.simulator

import org.scasmata.environment.{ActiveEntity, Dijkstra, Environment, Packet}

import org.scamata.core.{MATA, Allocation, Task, Worker}
import org.scamata.solver._
import org.scasmata.util.ParseUtils._

import scala.collection.SortedSet

/**
  * Scheduler which assigns packets to activeEntities
  **/
class Scheduler(e: Environment){
  val debug = true

  // Map ActiveEntity -> Plan
  private var assignment = Map[ActiveEntity, Seq[Packet]]()//Agent's references

  /**
    * Returns the targets of a specific activeEntity
    */
  def targets(idActiveEntity : Int) : Seq[Packet] = {
    assignment(e.activeEntities(idActiveEntity))
  }

  /**
    * Assign packets to the active entities and update packet colors
    */
  def assign() :  Unit = {
    val pb = buildMATA()
    if (debug) println(s"Scheduler assign pb: $pb")
    val solverMATA = new ECTSolver(pb,LCmax)
    if (debug) println(s"Scheduler assign allocation: $solverMATA")
    val allocation = solverMATA.run()
    generateAssignment(allocation)
  }

  /**
    * Build MATA from the environment
    */
  def buildMATA(): MATA = {
    val tasks: SortedSet[Task] = collection.immutable.SortedSet[Task]() ++ {
        for ((idPacket,_) <- e.packets) yield new Task(name = s"Packet$idPacket")
      }

    val workers: SortedSet[Worker] = collection.immutable.SortedSet[Worker]() ++ {
      for ((idActiveEntity,_) <- e.activeEntities) yield new Worker(name = s"ActiveEntity$idActiveEntity")
    }

    var costMatrix = Map[(Worker,Task), Double]()
    for(worker <-  workers){
      val (xs,ys) = e.location(e.activeEntities(name2id(worker.name))) //source
      val dijkstraSource = new Dijkstra(e,xs,ys)
      dijkstraSource.run()
      for (task <- tasks){
        val (xd,yd) = e.location(e.packets(name2id(task.name)))//destination
        val dijkstraDestination = new Dijkstra(e,xd,yd)
        dijkstraDestination.run()
        val costRound : Double  = dijkstraSource.distanceNeighbor(xd,yd)
        val costTrip : Double = dijkstraDestination.distanceNeighbor(e.destinationLocation()._1,e.destinationLocation()._2)
        val cost = costRound + costTrip + 2.0 // Pick up and drop down packet
        costMatrix = costMatrix + ( (worker,task) -> cost )
        }
      }
    if (debug) println(s"Scheduler buildMATA workers:  $workers")
    if (debug) println(s"Scheduler buildMATA tasks: $tasks")
    if (debug) println(s"Scheduler buildMATA costMatrix: $costMatrix")
    new MATA(workers, tasks, costMatrix)
}

  /**
    * Generate assignment from a MATA allocation and update the color of packets in the environment
    * @param allocation of a MATA
    */
  def generateAssignment(allocation: Allocation) : Unit ={
    for((worker,bundle) <- allocation.bundle){
      val idWorker = name2id(worker.name)
      var targets = Seq.empty[Packet]
      for (task <- bundle){
        val idTask = name2id(task.name)
        val packet = e.packets(idTask)
        targets = targets :+ packet
      }
      targets.foreach(packet =>e.updateTarget(idWorker, packet))
      assignment +=  (e.activeEntities(idWorker) -> targets)
    }
  }
}


