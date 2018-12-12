// Copyright (C) Maxime MORGE 2018
package org.scasmata.actor

import akka.actor.{Actor, ActorRef, FSM, Props}

import org.scasmata.environment.{Environment,AgentBody}

/**
  * The scheduler behaviour is described by a FSM with a single state
  */
sealed trait SchedulerState
case object InitialState extends SchedulerState

/**
  * Immutable state of the scheduler
  * @param influences to be applied
  */
class SchedulerStatus(val influences: Map[AgentBody,Influence])

/**
  * Scheduler which synchronizes the influences computes the reactions and updates the environment
  * @param environment current
  * */
class MAScheduler(environment: Environment) extends Actor with FSM[SchedulerState,SchedulerStatus] {
  var debug = true

  var executor : ActorRef = context.parent // Reference to the executor
  var directory = new Directory() // White page for body/actors
  var nbReady = 0 // Number of agents which are ready to talk to each other
  var nbFinished = 0 // Number of agents which have finished
  var steps = Map[Int,Int]() // Number of steps performed by the agents

  /**
    * Initially there is no influence
    */
  startWith(InitialState, new SchedulerStatus(Map[AgentBody,Influence]()))


  /**
    * Method invoked after starting the actor
    */
  override def preStart(): Unit = {
    environment.bodies().foreach{ body : AgentBody => //For all bodies
      if (debug) println(s"Scheduler: it creates an agent for body ${body.id}")
      val actor = context.actorOf(Props(classOf[AgentBehaviour], body.id), body.id.toString)
      directory.add(body, actor) // Add it to the directory
    }
  }

  /**
    * Message handling
    */
  when(InitialState) {
    //When job execution starts, it provides directory to all agents
    case Event(Start, status) =>
      executor = sender
      if (debug) println(s"Scheduler: it initiates all agents")
      directory.allActors().foreach( _ ! Init(directory))
      stay using status

    //When an agent becomes ready to talk
    case Event(Ready, status) =>
      nbReady += 1
      if (nbReady == environment.nbAgentBodies) {
        // When all of them are ready
        directory.allActors().foreach { actor: ActorRef => //Trigger them
          if (debug) println(s"Scheduler: it first updates ${directory.bodies(actor)}")
          actor ! Update(environment)
        }
      }
      stay using status

     //When an agent is finished
    case Event(Finished(step), status) =>
      val body = directory.bodies(sender)
      steps+= (body.id -> step)
      nbFinished += 1
      if (nbFinished == environment.nbAgentBodies) {
        // When all of them have finished
        directory.allActors().foreach { actor: ActorRef => //Stop them
          if (debug) println(s"Scheduler: it stops ${directory.bodies(actor)}")
          actor ! Stop
        }
        executor ! Outcome(steps)
        context.stop(self) // Stop the scheduler
      }
      stay using status

    //When an actor observe the environment
    case Event(Observe, status) =>
      val body = directory.bodies(sender)
      if (debug) println(s"Scheduler: it updates $body")
      sender ! Update(environment)
      stay using status

    // When an agent wants to move
    case Event(Move(direction), status) =>
      val body = directory.bodies(sender)
      if (debug) println(s"Scheduler: $body wants to move to $direction")
      // TODO compute the reactions
      stay using new SchedulerStatus(status.influences+(body -> Move(direction)))

    case Event(msg@_, status) =>
      println("Scheduler receives a message which was not expected: " + msg)
      stay using status
  }

  // Finally Triggering it up using initialize, which performs the transition into the initial state and sets up timers (if required).
  initialize()
}