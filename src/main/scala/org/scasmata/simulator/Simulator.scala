// Copyright (C) Maxime MORGE 2018
package org.scasmata.simulator

import scala.concurrent.duration.FiniteDuration
import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask

import scala.concurrent.Await
import akka.util.Timeout

import scala.concurrent.duration._
import scala.language.postfixOps
import org.scasmata.environment.{ActiveEntity, Center, Environment}
import org.scasmata.simulator.agent.Agent

/**
  * Simulator which :
  * - synchronizes the influences
  * - computes the reactions
  * - updates the environment
  * @param e current state of the environment
  * @param delay  waiting time before a reaction
  * */
class Simulator(val e: Environment, val delay : Int = 0) extends Actor{
  val debug = true
  // Default timeout of starting agent
  private val TIMEOUT_VALUE: FiniteDuration = 10 seconds
  implicit val timeout: Timeout = Timeout(TIMEOUT_VALUE)
  private var pause = false
  // The actor which triggers the simulation and gathers the steps
  private var runner : ActorRef= context.parent
  // White page id/agent
  private var directory = new Directory()
  // Number of simulation steps
  private var step = 0
  // Number of steps performed by the agents
  private var steps = Map[Int, Int]()
  e.bodies.foreach{ case (id,_) =>
    steps += (id -> 0)
  }
  // Map id/influence
  private var influences = Map[Int, Influence]()

  /**
    * Start simulator
    */
  e.bodies.values.foreach { body =>
    if (debug) println(s"Simulator creates an agent for body ${body.id}")
    val actor = context.actorOf(Props(classOf[Agent], body.id), body.id.toString)
    directory.add(body.id, actor) // Add it to the directory
  }

  /**
    *  Initiation of the agents with the directory
    */
  def init() : Unit = {
    if (debug) println(s"Simulator initiates all agents")
    directory.allAgents().foreach { a =>
      val future = a ? Init(directory)
      Await.result(future, timeout.duration) == Ready
      if (debug) println("Simulator receives ready")
    }
  }

  init()

  /**
    * Message handling
    */
  override def receive: Receive = {
    //When the simulator plays
    case Play =>
      runner = sender
      if (debug) println("Simulator runs")
      directory.allAgents().foreach { actor: ActorRef => //Trigger them
        actor ! Update(e)
      }
    //When the simulator is in Pause
    case Pause =>
      pause = true
    //When the simulator replays
    case Replay =>
      pause = false
      if (influences.keys.size == e.nbActiveEntities) { // Compute reaction
        triggerTimerIfRequired()
      } else {
        // Otherwise wait for other actions
        if (debug) println(s"Simulator waits for other influences")
      }
    //When the simulator plays next step
    case Next =>
      if (influences.keys.size == e.nbActiveEntities) { // Compute reaction
        triggerTimerIfRequired()
      } else {
        // Otherwise wait for other actions
        if (debug) println(s"Simulator waits for other influences")
      }
    //When the simulator is killed
    case Kill =>
      directory.allAgents().foreach(a => a ! Kill)
      context.stop(self)
    //When an actor observes the environment
    case Observe =>
      try {
        val id = directory.id(sender)
        if (debug) println(s"Simulator updates $id")
        sender ! Update(e)
      }catch {
        case _: Throwable => println("WARNING: Simulator does not update agents which are already dead")
      }
    //When an actor informs the simulator about its target
    case Inform(targets) =>
      val id = directory.id(sender)
      if (debug) println(s"Simulator updates $id's targets")
      targets.foreach(packet =>e.updateTarget(id, packet))
    // When an agent wants to act
    case influence: Influence =>
      val id = directory.id(sender)
      if (debug) println(s"Simulator receives $influence from $id")
      influences = influences + (id -> influence)
      // Count the steps
      if (influence != Move(Center))  steps += (id-> (steps.getOrElse(id,0)+1))
      if (influences.keys.size == e.nbActiveEntities && !pause) { // Compute reaction
        triggerTimerIfRequired()
      } else {
        // Otherwise wait for other actions
        if (debug) println(s"Simulator waits for other influences or replay")
      }
    // When the timeout for the reaction is received
    case Go =>
      if (debug) println(s"Simulator computes the reactions")
      val finished = react()
      if (debug) println(s"Stop $finished")
      if (finished) {
        stop()
      }
      influences = Map[Int, Influence]()
      step += 1

    case msg@_ =>
      println("WARNING: Simulator receives a message which was not expected: " + msg)
  }

  /**
    * Triggers a timer if required
    */
  def triggerTimerIfRequired() : Unit = {
    if (delay != 0) {
      val timer = context.actorOf(Props(classOf[Timer], delay), "timer" + Timer.nextId())
      timer ! Wait
    } else self ! Go
  }

  /**
    * Stops the simulator
    */
  def stop() : Unit = {
    if (debug) println("Simulator ends")
    reallocateSteps()
    runner ! Outcome(steps)
    directory.allAgents().foreach( _ ! Kill)
  }

  /**
    * Process each influence according to the physical laws of the environment
    * Returns true if all the packets are collected, false otherwise
    */
  def react() : Boolean = {
    val pickUps = influences.collect{ case (id, influence: PickUp) => (id, influence) }
    val putDowns = influences.collect{ case (id, influence : PutDown) => (id, influence) }
    val moves = influences.collect { case (id, influence : Move) => (id, influence) }
    val splits = influences.collect { case (id, influence : Split) => (id, influence) }

    val merges  = reciprocal(influences.collect{ case (id, influence : Merge) => (id, influence) } toList)

    // 1 - process pick up
    pickUps.foreach{
      case (id,PickUp(packet)) =>
        val entity = e.activeEntities(id)
        if (packet.size > entity.capacity || entity.load.isDefined || ! e.closedPacket(entity,packet)) {
          if (debug) println(s"Pickup(packet) by $entity failure")
          directory.adr(id) ! Failure
        }
        else {
          e.updatePickUp(entity, packet)
          if (debug) println(s"Pickup(packet) by $entity success")
          directory.adr(id) ! Success
        }
    }
    //2 - process put down
    putDowns.foreach{
      case (id, PutDown(packet)) =>
        val entity =  e.activeEntities(id)
        if (!entity.load.contains(packet) ||  !e.closedDestination(entity)) {
          if (debug) println(s"PutDown($packet) by $entity failure")
          directory.adr(id) ! Failure
        }
        else {
          e.updatePutDown(entity, packet)
          if (debug) println(s"PutDown($packet) by $entity success")
          directory.adr(id) ! Success
          if (e.isClean){
            if (debug) println(s"There is no more packets")
            return true
          }
          if (debug) println(s"There is still packets")
        }
    }
    //3- process moves
    moves.foreach{
      case (id,Move(direction)) =>
        val entity =  e.activeEntities(id)
        if (!e.isAccessibleDirection(entity,direction)){
          if (debug) println(s"Move($direction) by $entity failed")
          directory.adr(id) ! Failure
        }
        else{
          e.updateMove(entity,direction)
          if (debug) println(s"Move($direction) of $entity success")
          directory.adr(id) ! Success
        }
    }
    //4 - process merge
    merges.foreach {
      case (entity1, entity2) =>
        directory.adr(entity1.id) ! Kill
        directory.adr(entity2.id) ! Kill
        directory.remove(entity1.id, directory.adr(entity1.id))
        directory.remove(entity2.id, directory.adr(entity2.id))
        val crowd = e.updateMerge(entity1, entity2)
        val actor = context.actorOf(Props(classOf[Agent], crowd.id), crowd.id.toString)
        directory.add(crowd.id, actor) // Add it to the directory
        init()
        actor ! Update(e)
    }

    //5 - TODO process split
    splits.foreach{
      case (id,Split()) =>
        val newBodies = e.updateSplit(e.crowds(id))
        if (newBodies.isEmpty) directory.adr(id) ! Failure
        directory.adr(id) ! Kill
        directory.remove(id, directory.adr(id))
        newBodies.foreach{ b=>
          val actor = context.actorOf(Props(classOf[Agent], b.id), b.id.toString)
          directory.add(b.id, actor) // Add it to the directory
        }
        init()
        newBodies.foreach { b =>
          directory.adr(b.id) ! Update(e)
        }
    }
    false
  }

  /**
    * Return the reciprocal merges
    */
  def reciprocal(merges: List[(Int, Merge)]) : Seq[(ActiveEntity,ActiveEntity)] = {
    if (merges.isEmpty) return Seq()
    val (id, influence) = merges.head
    val origin = e.bodies.getOrElse(id,e.crowds(id))
    val target = influence.entity
    val tail = merges.tail
    if (tail.contains((target.id,Merge(origin))) &&
      e.closedActiveEntity(origin, target) &&
      origin.load.isEmpty && target.load.isEmpty
    ) {
      return reciprocal(tail) :+ (origin, target)
    }
    if (debug) println(s"Merge($target) by $origin failure")
    directory.adr(origin.id) ! Failure
    reciprocal(tail)
  }

  /**
    * Reallocate the steps of crowds toward the bodies
    */
  def reallocateSteps() : Unit = {
    // for each crowd
    e.crowds.filterKeys(_ > e.n).foreach{ case (crowdId,_) =>
      // for each body within the crowd
      e.crowds(crowdId).bodies.foreach{ body =>
        steps += (body.id -> (steps.getOrElse(body.id,0)+steps.getOrElse(crowdId,0)))
      }
    }
    // filter steps for only bodies
    steps = steps.filterKeys( _ <= e.n)
  }
}

/**
  * Companion object for class variable
  */
object Simulator {
  var id = 0
  def nextId() : Int = {
    id += 1
    id
  }
}
