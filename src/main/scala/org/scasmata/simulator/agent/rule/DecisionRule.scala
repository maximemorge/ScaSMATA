// Copyright (C) Maxime MORGE 2018
package org.scasmata.simulator.agent.rule

import java.util.concurrent.ThreadLocalRandom

import org.scasmata.simulator._
import org.scasmata.environment._
import org.scasmata.simulator.agent.Mind

/**
  * Abstract decision rule
  */
trait DecisionRule{
  val debug = true
  val rnd : ThreadLocalRandom = ThreadLocalRandom.current()

  /**
    * Decides the next move for agent id with the mind
    */
  def decide(id: Int, mind: Mind) : Influence
}
