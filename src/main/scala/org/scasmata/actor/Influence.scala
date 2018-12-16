// Copyright (C) Maxime MORGE 2018
package org.scasmata.actor

import org.scasmata.environment.{Environment,Direction,Color}

/**
  * ManagingMessage
  */
abstract class ManagingMessage
case object Play extends ManagingMessage
case object Pause extends ManagingMessage
case object Replay extends ManagingMessage
case class Init(d: Directory) extends ManagingMessage // Provide directory to agents
case object QueryResult extends ManagingMessage
case class Result(steps: Int) extends ManagingMessage // Agent stops after a number of steps
case class Outcome(steps: Map[Int,Int]) extends ManagingMessage // Simulator finished with steps for each agent
case object Wait extends ManagingMessage // Start timer
case object Go extends ManagingMessage // Timeout from a timer

/**
  * Message between the scheduler and the agent
  */
abstract class Message
case object Ready extends Message // The agent is ready to talk to the other agents
abstract class ObservationMessage extends Message
case object Observe extends ObservationMessage // The agent observe the environment throught the scheduler
case class Update(environment : Environment) extends ObservationMessage // The agent is informed about the environment state

/**
  * Influence
  */
abstract class Influence // The agent want to
case class Move(direction : Direction) extends Influence() { // move toward a particular direction
  override def toString: String = s"Move($direction)"
}
case class PickUp(idPacket : Int) extends Influence() { // pick up a particular packet
  override def toString: String = s"PickUp(packet$idPacket)"
}
case class PutDown(idPacket : Int, color: Color) extends Influence() { // put down a particular packet in a colored destination
  override def toString: String = s"PutDown(packet$idPacket)"
}

/**
  * Reaction
  */
abstract class Reaction // The reaction of an influence can be
case object Success extends Reaction // positive
case object Failure extends Reaction// or negative
