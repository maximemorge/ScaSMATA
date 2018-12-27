// Copyright (C) Maxime MORGE 2018
package org.scasmata.simulator

import scala.concurrent.duration.FiniteDuration
import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask

import scala.concurrent.Await
import akka.util.Timeout

import scala.concurrent.duration._
import scala.language.postfixOps
import org.scasmata.environment.{Center, Environment}
import org.scasmata.simulator
import org.scasmata.simulator.agent.ProactiveAgent

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
  val TIMEOUT_VALUE: FiniteDuration = 1 seconds // Default timeout of starting agent
  implicit val timeout: Timeout = Timeout(TIMEOUT_VALUE)
  var pause = false

  var runner : ActorRef= context.parent // The actor which triggers the simulation and gathers the steps
  var directory = new Directory() // White page id/agent
  var nbReadyAgent = 0 // Number of agents which are ready to talk to each other
  var nbStoppedAgents = 0
  var step = 0 // Number of simulation steps
  var steps = Map[Int, Int]() // Number of steps performed by the agents
  var influences = Map[Int, Influence]() // Map id/influence

  /**
    * Start simulator
    */
  e.bodies.values.foreach { body =>
    if (debug) println(s"Simulator creates an agent for body ${body.id}")
    val actor = context.actorOf(Props(classOf[ProactiveAgent], body.id), body.id.toString)
    directory.add(body.id, actor) // Add it to the directory
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
      if (influences.keys.size == e.n) { // Compute reaction
        triggerTimerIfRequired()
      } else {
        // Otherwise wait for other actions
        if (debug) println(s"Simulator waits for other influences")
      }

    //When the simulator play next step
    case Next =>
      if (influences.keys.size == e.n) { // Compute reaction
        triggerTimerIfRequired()
      } else {
        // Otherwise wait for other actions
        if (debug) println(s"Simulator waits for other influences")
      }

    //When the simulator is killed
    case Kill =>
      directory.allAgents().foreach(a => a ! Kill)
      context.stop(self)

    //When an actor observe the environment
    case Observe =>
      val bodyId = directory.id(sender)
      if (debug) println(s"Simulator updates $bodyId")
      sender ! Update(e)

    //When an actor observe the environment
    case Inform(targets) =>
      val bodyId = directory.id(sender)
      if (debug) println(s"Simulator updates $bodyId's targets")
      targets.foreach(packet =>e.updateTarget(bodyId, packet))

    // When an agent wants to act
    case influence: Influence =>
      val bodyId = directory.id(sender)
      if (debug) println(s"Simulator receives $influence from $bodyId")
      influences = influences + (bodyId -> influence)
      // Count the steps
      if (influence != Move(Center))  steps += (bodyId-> (steps.getOrElse(bodyId,0)+1))
      if (influences.keys.size == e.n && !pause) { // Compute reaction
        triggerTimerIfRequired()
      } else {
        // Otherwise wait for other actions
        if (debug) println(s"Simulator waits for other influences or replay")
      }

    // When the timeout for the reaction is received
    case Go =>
      if (debug) println(s"Simulator: it computes the reactions")
      val finished = react()
      if (debug) println(s"Stop $finished")
      if (finished) {
        stop()
      }
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
    *
    * @param directory
    */
  def stop() : Unit = {
    if (debug) println("Simulator ends")
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

    // 1 - process pick up
    pickUps.foreach{
      case (id,PickUp(packet)) =>
        val body = e.bodies(id)
        if (packet.size > 1 || body.load.isDefined || ! e.closedPacket(body,packet)) {
          if (debug) println(s"Pickup(packet) by $body failure")
          directory.adr(id) ! Failure
        }
        else {
          e.updatePickUp(body, packet)
          if (debug) println(s"Pickup(packet) by $body success")
          directory.adr(id) ! Success
        }
      case (id,influence) =>
        val body = e.bodies(id)
        throw new RuntimeException(s"$influence by $body was not excepted")
    }

    //2 - process put down
    putDowns.foreach{
      case (id, PutDown(packet)) =>
        val body = e.bodies(id)
        if (!body.load.contains(packet) ||  !e.closedDestination(body)) {
          if (debug) println(s"PutDown($packet) by $body failure")
          directory.adr(id) ! Failure
        }
        else {
          e.updatePutDown(body, packet)
          if (debug) println(s"PutDown($packet) by $body success")
          directory.adr(id) ! Success
          if (e.isClean()){
            if (debug) println(s"There is no more packets")
            return true
          }
          if (debug) println(s"There is still packets")
        }
      case (id,influence) =>
        val body = e.bodies(id)
        throw new RuntimeException(s"$influence by $body was not excepted")
    }

    //3 - process moves
    moves.foreach{
      case (id,Move(direction)) =>
        val body = e.bodies(id)
        if (!e.isAccessibleDirection(body,direction)){
          if (debug) println(s"Move($direction) by $body failed")
          directory.adr(id) ! Failure
        }
        else{
          e.updateMove(body,direction)
          if (debug) println(s"Move($direction) of $body success")
          directory.adr(id) ! Success
        }
      case (id,influence) =>
        val body = e.bodies(id)
        throw new RuntimeException(s"$influence by $body was not excepted")
    }
    false
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
