// Copyright (C) Maxime MORGE 2018
package org.scasmata.actor

import org.scasmata.environment.{Environment,Direction}

/**
  * Message betwen the MAScheduler and the agents
  */
abstract class Message

/**
  * ManagingMessage
  */
abstract class ManagingMessage extends Message
case object Start extends ManagingMessage
case object Stop extends ManagingMessage
case class Init(d: Directory) extends ManagingMessage
case class Finished(steps: Int) extends ManagingMessage
case class Outcome(steps: Map[Int,Int]) extends ManagingMessage

/**
  * Message between the scheduler and the agent
  */
case object Ready extends Message // The agent is ready to talk to the other agents
abstract class ObservationMessage extends Message
case object Observe extends ObservationMessage // The agent observe the environment throught the scheduler
case class Update(environment : Environment) extends ObservationMessage // The agent is informed about the environment state

/**
  * Influence
  */
abstract class Influence extends Message
case class Move(direction : Direction) extends Influence // The agent want to move toward a particular direction

/**
  * Reaction
  */
abstract class Reaction extends Message

