// Copyright (C) Maxime MORGE 2018
package org.scasmata.actor

import scala.concurrent.duration.FiniteDuration
import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import scala.concurrent.Await
import akka.util.Timeout
import scala.concurrent.duration._
import scala.language.postfixOps

import org.scasmata.environment.{Center, Environment}

/**
  * Simulator which :
  * - synchronizes the influences
  * - computes the reactions
  * - updates the environment
  * @param environment current state of the environment
  * @param delay  waiting time before a reaction
  * */
class Simulator(val e: Environment, val delay : Int = 0) extends Actor{
  val debug = true

  val TIMEOUTVALUE: FiniteDuration = 1 seconds // Default timeout of step
  implicit val timeout: Timeout = Timeout(TIMEOUTVALUE)
  var pause = false

  var runner = context.parent // The actor which triggers the simulation and gathers the steps
  var directory = new Directory() // White page for bodyId/ActorRef
  var nbReadyAgent = 0 // Number of agents which are ready to talk to each other
  var nbStoppedAgents = 0
  var step = 0 // Number of simulation steps
  var steps = Map[Int, Int]() // Number of steps performed by the agents
  var influences = Map[Int, Influence]()

  /**
    * Start simulator
    */
  e.bodyIds().foreach { bodyId =>
    if (debug) println(s"Simulator creates an agent for body $bodyId")
    val actor = context.actorOf(Props(classOf[Agent], bodyId), bodyId.toString)
    directory.add(bodyId, actor) // Add it to the directory
  }
  // Initiation of the agents with the directory
  if (debug) println(s"Simulator initiates all agents")
  directory.allAgents().foreach { a =>
    val future = a ? Init(directory)
    Await.result(future, timeout.duration) == Ready
    if (debug) println("Simulator receives ready")
  }

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
      if (influences.keys.size == e.nbAgentBodies) { // Compute reaction
        triggerTimerIfRequired()
      } else {
        // Otherwise wait for other actions
        if (debug) println(s"Simulator: it waits for other influences")
      }

    //When the simulator play next step
    case Next =>
      if (influences.keys.size == e.nbAgentBodies) { // Compute reaction
        triggerTimerIfRequired()
      } else {
        // Otherwise wait for other actions
        if (debug) println(s"Simulator: it waits for other influences")
      }

    //When the simulator is killed
    case Kill =>
      directory.allAgents.foreach(a => a ! Kill)
      context.stop(self)

    //When an actor observe the environment
    case Observe =>
      val bodyId = directory.id(sender)
      if (debug) println(s"Simulator updates $bodyId")
      sender ! Update(e)

    // When an agent wants to act
    case influence: Influence =>
      val bodyId = directory.id(sender)
      if (debug) println(s"Simulator receives $influence from $bodyId")
      influences = influences + (bodyId -> influence)
      if (influence != Move(Center))  steps += (bodyId-> (steps.getOrElse(bodyId,0)+1))
      if (influences.keys.size == e.nbAgentBodies && !pause) { // Compute reaction
        triggerTimerIfRequired()
      } else {
        // Otherwise wait for other actions
        if (debug) println(s"Simulator: it waits for other influences or replay")
      }

    // When the timeout for the reaction is received
    case Go =>
      if (debug) println(s"Simulator: it computes the reactions")
      react()
      influences = Map[Int, Influence]()
      step += 1

    case msg@_ =>
      println("Simulator: it receives a message which was not expected: " + msg)
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
    * Process each influence according to the physical laws of the environment
    */
  def react() : Unit = {
    influences.map{

      case (bodyId,Move(direction)) =>
        if (!e.isPossibleDirection(bodyId,direction)){
          if (debug) println(s"Move($direction) of $bodyId is impossible")
          directory.adr(bodyId) ! Failure
        }
        else{
          e.updateMove(bodyId,direction)
          if (debug) println(s"Move($direction) of $bodyId is performed")
          directory.adr(bodyId) ! Success
        }

      case (bodyId,PickUp(id)) =>
        val listOfPacket = e.closedPackets(bodyId)
        if (e.load(bodyId) != 0 || listOfPacket.isEmpty) {
          if (debug) println(s"Pickup($id) of $bodyId is failed")
          directory.adr(bodyId) ! Failure
        }
        else {
          val idPacket = listOfPacket.head
          e.updatePickUp(bodyId,idPacket)
          if (debug) println(s"Pickup($idPacket) of $bodyId is performed")
          directory.adr(bodyId) ! Success
        }

      case (bodyId, PutDown(idPacket,color)) =>
        val listOfDestination = e.closedDestinations(bodyId,color)
        if (e.load(bodyId)==0 || listOfDestination.isEmpty) {
          if (debug) println(s"PutDown($idPacket,$color) of $bodyId is failed")
          directory.adr(bodyId) ! Failure
        }
        else {
          e.updatePutDown(bodyId,idPacket)
          if (debug) println(s"PutDown($idPacket) of $bodyId is performed")
          directory.adr(bodyId) ! Success
          if (e.nbScatteredPackets == 0){
            if (debug) println(s"No more packets")
            killAgents(directory)
          }
          if (debug) println(s"There is still packets")
        }
      case (bodyId,influence) =>
        new RuntimeException(s"Reactor: $influence by $bodyId was not excepted")
    }
  }

  def killAgents(directory : Directory) = {
    runner ! Outcome(steps)
    directory.allAgents().foreach( _ ! Kill)
  }
}

/**
  * Companion object for class variable
  */
object Simulator {
  var id = 0
  def nextId() = {
    id += 1
    id
  }
}
