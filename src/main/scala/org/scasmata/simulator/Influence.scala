// Copyright (C) Maxime MORGE 2018
package org.scasmata.simulator

import org.scasmata.environment.{Environment,Direction,Packet}

/**
  * ManagingMessage
  */
abstract class ManagingMessage
case object Play extends ManagingMessage
case object Pause extends ManagingMessage
case object Replay extends ManagingMessage
case object Next extends ManagingMessage
case object Kill extends ManagingMessage
case class Init(d: Directory) extends ManagingMessage // Provides directory to agents
case class Outcome(steps: Map[Int,Int]) extends ManagingMessage // Simulator finished with steps for each agent
case object Wait extends ManagingMessage // Start timer
case object Go extends ManagingMessage // Timeout from a timer

/**
  * Message between the scheduler and the agent
  */
abstract class Message
case object Ready extends Message // The agent is ready to talk to the other agents
abstract class ObservationMessage extends Message
case object Observe extends ObservationMessage // The agent observe the environment through the simulator
case class Update(environment : Environment) extends ObservationMessage // The agent is informed about the environment state
case class Inform(targets: Seq[Packet]) extends  ObservationMessage// The agents informs the simulator about its targets

/**
  * Influence
  */
abstract class Influence // The agent want to
case class Move(direction : Direction) extends Influence() { // move toward a particular direction
  override def toString: String = s"Move($direction)"
}
case class PickUp(packet: Packet) extends Influence() { // pick up a particular packet
  override def toString: String = s"PickUp(packet$packet)"
}
case class PutDown(packet: Packet) extends Influence() { // put down a particular packet in a colored destination
  override def toString: String = s"PutDown(packet)"
}

/**
  * Reaction
  */
abstract class Reaction // The reaction of an influence can be
case object Success extends Reaction // positive
case object Failure extends Reaction// or negative
