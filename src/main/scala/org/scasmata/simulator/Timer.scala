// Copyright (C) Maxime MORGE 2018
package org.scasmata.simulator


import akka.actor.{ Actor, ReceiveTimeout }
import scala.concurrent.duration._
import scala.language.postfixOps

/**
  * Timer actor which make a countdown.
  * @param duration duration of the timer in milliseconds
  */
class Timer(duration: Int) extends Actor{

  /** @see akka.actor.Actor.receive() */
  def receive: Receive = this.active orElse this.handleUnexpected

  /** Active state of the timer. */
  def active: Receive = {
    // turn it on
    case Wait =>
      context.setReceiveTimeout(Duration(this.duration, MILLISECONDS))

    case ReceiveTimeout =>
      // turn it off
      context.setReceiveTimeout(Duration.Undefined)
      context.parent ! Go
      context stop self
  }

  /** Handle unexpected messages */
  def handleUnexpected: Receive = {
    case msg@_ =>
      new RuntimeException(s"Timer receives $msg by $sender which was not excepted")
  }
}

/**
  * Companion object for class variable
  */
object Timer {
  var id = 0
  def nextId() : Int = {
    id += 1
    id
  }
}
