// Copyright (C) Maxime MORGE 2019
package org.scasmata.util

/**
  * Behaviour of the operational agent
  */
sealed abstract class Behaviour extends Product with Serializable{
  override def toString: String = this match{
    case Proactive => "ProactiveBehaviour"
    case Reactive => "ReactiveBehaviour"
  }
}
case object Proactive extends Behaviour
case object Reactive extends Behaviour

object Behaviour{
  val behaviours : Seq[Behaviour] = Seq(Proactive, Reactive)
}

/**
  * Rule for scheduling the gathering round
  */
sealed abstract class SchedulingRule extends Product with Serializable{
  override def toString: String = this match{
    case RandomRule => "Random"
    case ECTRule => "Early completion time"
    case NoRule => "None"
    case GiftRule => "Gift"
    case SwapRule => "Swap"
    case SwapAndGiftRule => "Gift & Swap"
  }
}
case object RandomRule extends SchedulingRule
case object ECTRule extends SchedulingRule
case object NoRule extends SchedulingRule
case object GiftRule extends SchedulingRule
case object SwapRule extends SchedulingRule
case object SwapAndGiftRule extends SchedulingRule

object SchedulingRule{
  val schedulingRules: Seq[SchedulingRule] = Seq(ECTRule, RandomRule, NoRule, GiftRule, SwapRule, SwapAndGiftRule)
}


/**
  * Class representing a simulation configuration
  * @param height of the environment (8 by default)
  * @param width of the environment (16 by default)
  * @param n number of bodies (4 by default)
  * @param m number of packets (8 by default)
  * @param minSizePackets minimal size of the packets (1 by default)
  * @param maxSizePackets maximal size of the packets (1 by default)
  * @param behaviour of the operational agent (Proactive by default)
  * @param rule for scheduling the gathering round
  */
class Configuration(val height: Int = 8, val width: Int = 16, val n: Int = 4, val m: Int = 8, val minSizePackets: Int = 1, val maxSizePackets: Int = 1,
                    val behaviour: Behaviour = Proactive, val rule : SchedulingRule = SwapAndGiftRule) {
}

