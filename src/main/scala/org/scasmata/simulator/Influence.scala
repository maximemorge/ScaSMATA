// Copyright (C) Maxime MORGE 2018
package org.scasmata.simulator

import akka.actor.ActorRef
import org.scasmata.environment.{Direction, Environment, Packet}

/**
  * ManagingMessage
  */
abstract class ManagingMessage
case object Play extends ManagingMessage
case object Pause extends ManagingMessage
case object Replay extends ManagingMessage
case object Next extends ManagingMessage
case object Kill extends ManagingMessage
case class Init(d: Directory) extends ManagingMessage // Provide directory to agents
case class Outcome(steps: Map[Int,Int]) extends ManagingMessage // Simulator finished with steps for each agent
case object Wait extends ManagingMessage // Start timer
case object Go extends ManagingMessage // Timeout from a timer

/**
  * Internal Message
  */
abstract class InternalMessage
case object Operate extends InternalMessage // Init the worker
case class Delegate(target: Option[Packet], environment: Environment) extends InternalMessage // delegate the target collection to the worker
case class Done(target : Packet) extends InternalMessage // worker informs the agent that target is collected
case class QueryTargets(vision: Environment) extends InternalMessage // Query the negotiator about targets
case class ReplyTargets(targets: Seq[Packet]) extends InternalMessage // The negotiator provides targets
case class QueryPartner(target: Packet) extends InternalMessage // Query the negotiator about a partner
case class ReplyPartner(target: Packet, partnerId : Int) extends InternalMessage // The negotiator has found a partner

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
