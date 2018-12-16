// Copyright (C) Maxime MORGE 2018
package org.scasmata.actor

import org.scasmata.environment.{AgentBody, Environment}
import org.scasmata.util.UI

import scala.concurrent.duration.FiniteDuration
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import scala.concurrent.Await
import akka.util.Timeout
import scala.concurrent.duration._
import scala.language.postfixOps

/**
  * Simulator which :
  * - synchronizes the influences
  * - computes the reactions
  * - updates the environment
  * @param environment current state of the environment
  * @param delay  waiting time before a reaction
  * */
class Simulator(val e: Environment, val delay : Int = 0) extends Actor with Reactor {
  override val debug = true

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
  //override def preStart(): Unit = {
    // Creation of the agents and directory update
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
  //}

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
      if (influences.keys.size == e.nbAgentBodies){ // Compute reaction
        val timer = context.actorOf(Props(classOf[Timer],delay), "timer")
        timer ! Wait
      }else {// Otherwise wait for other actions
        if (debug) println(s"Simulator: it waits for other influences")
      }

    //When the simulator play next step
    case Next =>
      if (influences.keys.size == e.nbAgentBodies){ // Compute reaction
        val timer = context.actorOf(Props(classOf[Timer],delay), "timer")
        timer ! Wait
      }else {// Otherwise wait for other actions
        if (debug) println(s"Simulator: it waits for other influences")
      }

    //When the simulator is killed
    case Kill =>
      directory.allAgents.foreach(a => a ! Kill)
      context.stop(self)

    //When the simulator is informed about the number of step of an agent
    case Result(step) =>
      val bodyId = directory.id(sender)
      steps += (bodyId -> step)
      nbStoppedAgents += 1
      if (nbStoppedAgents == e.nbAgentBodies) {
        runner ! Outcome(steps)
        context.stop(self)
      }

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
      if (influences.keys.size == e.nbAgentBodies &&  !pause){ // Compute reaction
        val timer = context.actorOf(Props(classOf[Timer],delay), "timer")
        timer ! Wait
      }else {// Otherwise wait for other actions
        if (debug) println(s"Simulator: it waits for other influences or replay")
      }

    // When the timeout for the reaction is received
    case Go =>
      if (debug) println(s"Simulator: it computes the reactions")
      react(influences, e, directory)
      step += 1

    case msg@_ =>
      println("Simulator: it receives a message which was not expected: " + msg)
  }

}

object Simulator {
  var id = 0
  def nextId() = {
    id += 1
    id
  }
}
