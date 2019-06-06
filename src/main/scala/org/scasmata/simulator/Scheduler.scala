// Copyright (C) Maxime MORGE, Florian LECOINTE, Quentin BRIAND  2019
package org.scasmata.simulator

import org.scasmata.util.{ECTRule, GiftRule, RandomRule, SchedulingRule, SwapAndGiftRule, SwapRule}
import org.scasmata.util.ParseUtils._
import org.scasmata.environment.{ActiveEntity, Dijkstra, Environment, Packet}
import org.scamata.core.{Allocation, MATA, Task, Worker}
import org.scamata.solver._

import scala.collection.SortedSet

/**
  * Scheduler which assigns packets to activeEntities
  * @param env where packets are spread
  * @param rule to choose the MATA solver
  **/
class Scheduler(env: Environment, rule : SchedulingRule){
  val debug = false

  // Each active entity is assigned to a plan, i.e. a list of packets spread in the environment
  private var assignment = Map[ActiveEntity, Seq[Packet]]()// the entity id corresponds to the agent id


  /**
    * Return the targets of a specific active entity
    * @param id of the activeEntity
    */
  def targets(id : Int) : Seq[Packet] = {
    assignment(env.activeEntities(id))
  }

  /**
    * Returns the allocation which is the abstraction of the assignment
    * @param pb corresponding the allocation
    */
  def currentAllocation(pb : MATA) : Allocation = {
    val allocation = new Allocation(pb)
    pb.workers.foreach{ w =>
      val entity = env.activeEntities(name2id(w.name))
      var b = SortedSet.empty[Task]
      pb.tasks.foreach { t =>
          val packet = env.packets(name2id(t.name))
          if (assignment(entity).contains(packet)) b += t
      }
      allocation.bundle += (w -> b)
    }
    allocation
  }

  /**
    * Assign packets to the active entities and update packet colors
    */
  def assign() :  Unit = {
    val pb = buildMATA()
    if (debug) println(s"Scheduler assign pb: $pb")
    val solverMATA = rule match {
      case ECTRule => new ECTSolver(pb,LCmax)
      case RandomRule => new RandomSolver(pb,LCmax)
      case GiftRule => new CentralizedSolver(pb,LCmax,SingleGiftOnly)
      case SwapRule => new CentralizedSolver(pb,LCmax,SingleSwapOnly)
      case SwapAndGiftRule => new CentralizedSolver(pb,LCmax,SingleSwapAndSingleGift)
      case _ => throw new RuntimeException("Scheduling rule does match to a MATA Solver")
    }
    if (debug) println(s"Scheduler assign allocation: $solverMATA")
    rule match {
      case GiftRule | SwapRule | SwapAndGiftRule=> solverMATA.asInstanceOf[CentralizedSolver].trace = true
      case _ =>
    }
    val allocation = solverMATA.run()
    generateAssignment(pb,allocation)
  }

  /**
    * Reassign packets to the active entities and update packet colors
    */
  def reassign() : Unit = {
    val pb = buildMATA()
    if (debug) println(s"Scheduler assign pb: $pb")
    val solverMATA : Solver = rule match {
      case ECTRule => new ECTSolver(pb,LCmax)
      case RandomRule => new RandomSolver(pb,LCmax)
      case GiftRule => new CentralizedSolver(pb,LCmax,SingleGiftOnly)
      case SwapRule => new CentralizedSolver(pb,LCmax,SingleSwapOnly)
      case SwapAndGiftRule => new CentralizedSolver(pb,LCmax,SingleSwapAndSingleGift)
      case _ => throw new RuntimeException("Scheduling rule does match to a MATA Solver")
    }
    if (debug) println(s"Scheduler reassign allocation: $solverMATA")
    rule match {
      case GiftRule | SwapRule | SwapAndGiftRule => solverMATA.asInstanceOf[CentralizedSolver].trace = true
      case _ =>
    }
    val allocation = rule match {
      case GiftRule | SwapRule | SwapAndGiftRule => solverMATA.asInstanceOf[DealSolver].reallocate(currentAllocation(pb))
      case ECTRule | RandomRule  => solverMATA.run()
      case _ => throw new RuntimeException("Scheduling rule does match to a MATA Solver")
    }
    generateAssignment(pb,allocation)
  }

  /**
    * Returns the MATA from the environment
    */
  def buildMATA(): MATA = {
    // One task per packet
    val tasks: SortedSet[Task] = collection.immutable.SortedSet[Task]() ++ {
        for ((idPacket,_) <- env.packets) yield new Task(name = s"Packet$idPacket")
      }
    // One worker per active entity
    val workers: SortedSet[Worker] = collection.immutable.SortedSet[Worker]() ++ {
      for ((idActiveEntity,_) <- env.activeEntities) yield new Worker(name = s"ActiveEntity$idActiveEntity")
    }
    // Cost matrix
    var costMatrix = Map[(Worker,Task), Double]()
    val (xd,yd) = env.destinationLocation()
    for(worker <-  workers){
      val activeEntity = env.activeEntities(name2id(worker.name))
      val (xs,ys) = env.location(activeEntity) // source
      val dijkstraSource = new Dijkstra(env,xs,ys)
      dijkstraSource.run()
      // effort to bring back the loaded packet
      val costCurrentTask = if (activeEntity.isLoaded) dijkstraSource.distanceNeighbor(xd,yd)+ 1 else 0
      for (task <- tasks){
        val (xt,yt) = env.location(env.packets(name2id(task.name))) // target
        val dijkstraPacket = new Dijkstra(env,xt,yt)
        dijkstraPacket.run()
        // bring back the next packet
        val costTrip : Double = dijkstraPacket.distanceNeighbor(xd,yd)
        // reach the next packet even if the current task is finished
        val costRound : Double  = if (!activeEntity.isLoaded) dijkstraSource.distanceNeighbor(xt,yt) else costTrip
        // pick it up + reach destination + drop packet
        val cost = costCurrentTask + costRound + 1.0 + costTrip + 1.0
        costMatrix = costMatrix + ( (worker,task) -> cost )
        }
      }
    if (debug) println(s"Scheduler buildMATA costMatrix: $costMatrix")
    new MATA(workers, tasks, costMatrix)
  }

  /**
    * Generates the assignment
    * @param pb which is the abstraction of the current environment
    * @param allocation which is the abstraction of the assignment
    */
  def generateAssignment(pb : MATA, allocation: Allocation) : Unit ={
    for((worker,bundle) <- allocation.bundle){
      // Sort the bundle in decreasing order of cost
      val orderedBundle : Seq[Task] = bundle.toSeq.sortWith( pb.costMatrix(worker,_) < pb.costMatrix(worker,_))
      val idWorker = name2id(worker.name)
      var targets = Seq.empty[Packet]
      for (task <- orderedBundle){
        val idTask = name2id(task.name)
        val packet = env.packets(idTask)
        targets = targets :+ packet
      }
      targets.foreach(packet =>env.updateTarget(idWorker, packet))// Color the packets
      assignment +=  (env.activeEntities(idWorker) -> targets)
    }
  }
}