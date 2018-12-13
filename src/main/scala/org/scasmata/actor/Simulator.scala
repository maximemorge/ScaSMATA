// Copyright (C) Maxime MORGE 2018
package org.scasmata.actor

import org.scasmata.environment.{AgentBody, Environment}

import scala.concurrent.duration.FiniteDuration
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import scala.concurrent.Await
import akka.util.Timeout
import scala.concurrent.duration._
import scala.language.postfixOps


/**
  * Scheduler which :
  * - synchronizes the influences
  * - computes the reactions
  * - updates the environment
  * @param environment current state of the environment
  * */
class Simulator(val e: Environment) extends Actor with Reactor {
  override val debug = true

  var runner = context.parent

  val TIMEOUTVALUE : FiniteDuration = 6000 minutes // Default timeout of a run
  implicit val timeout : Timeout = Timeout(TIMEOUTVALUE)

  var directory = new Directory() // White page for body/actors
  var nbReady = 0 // Number of agents which are ready to talk to each other
  var nbFinished = 0
  var step = 0 // Number of simulation steps
  var steps = Map[Int,Int]() // Number of steps performed by the agents

  var influences = Map[AgentBody,Influence]()


  //Start
  e.bodies().foreach{ body : AgentBody => //For all bodies
    if (debug) println(s"Simulatore: it creates an agent for body ${body.id}")
    val actor = context.actorOf(Props(classOf[AgentBehaviour], body.id), body.id.toString)
    directory.add(body, actor) // Add it to the directory
  }
  if (debug) println(s"Scheduler: it initiates all agents")
  directory.allActors().foreach{ a =>
    val future = a ? Init(directory)
    Await.result(future, timeout.duration) == Ready
  }


  override def receive: Receive = {
    //When the simulator is start
    case Run =>
      runner = sender
      if (debug) println("Start Simulator")
      directory.allActors().foreach { actor: ActorRef => //Trigger them
        if (debug) println(s"Scheduler: it first updates ${directory.bodies(actor)}")
        actor ! Update(e)
      }
      if (debug) println("End SingleExecutor")

    //When an agent is finished
    case Result(step) =>
      val body = directory.bodies(sender)
      steps += (body.id -> step)
      nbFinished += 1
      if (nbFinished == e.nbAgentBodies) {
        // When all of them have finished
        directory.allActors().foreach { actor: ActorRef => //Stop them
          if (debug) println(s"Scheduler: it stops ${directory.bodies(actor)}")
          actor ! Stop
        }
        runner ! Outcome(steps)
        context.stop(self) // Stop the scheduler
      }

    //When an actor observe the environment
    case Observe =>
      val body = directory.bodies(sender)
      if (debug) println(s"Scheduler: it updates $body")
      sender ! Update(e)

    // When an agent wants to act
    case influence: Influence =>
      val body = directory.bodies(sender)
      if (debug) println(s"Scheduler: $body wants to $influence")
      influences = influences + (body -> influence)
      if (influences.keys.size == e.nbAgentBodies){ // Compute reaction
        if (debug) println(s"Simulator: it computes the reactions")
        react(influences, e, directory)
        step += 1
      }else {
        // Otherwise wait for other actions
        if (debug) println(s"Simulator: it waits for other influences")
      }

    case msg@_ =>
      println("Simulator: it receives a message which was not expected: " + msg)
  }

}


