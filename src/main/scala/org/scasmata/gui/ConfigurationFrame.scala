// Copyright (C) Maxime MORGE 2019
package org.scasmata.gui

import akka.actor.{ActorSystem, Props}
import org.scasmata.util._

import scala.language.postfixOps
import scala.swing._
import scala.swing.event.SelectionChanged

/**
  * Class representing the configuration frame
  *
  * @param configuration of the simulation
  */
class ConfigurationFrame(configuration: Configuration) extends MainFrame {
  val debug = true
  title = "ScaSMATA configuration"

  // Setup the environment based on the current configuration
  var width = new ComboBox((8 to 16 by 2).reverse)
  width.selection.item = configuration.width
  var height = new ComboBox((4 to 8).reverse)
  height.selection.item = configuration.height
  val n = new ComboBox((1 to 4).reverse)
  n.selection.item = configuration.n
  val m = new ComboBox((1 to 8).reverse)
  m.selection.item = configuration.m
  val minSizePackets = new ComboBox(1 to 4)
  minSizePackets.selection.item = configuration.minSizePackets
  val maxSizePackets = new ComboBox(1 to 4)
  maxSizePackets.selection.item = configuration.maxSizePackets

  // Setup the simulation  based on the current configuration
  val behaviour = new ComboBox(Behaviour.behaviours)
  behaviour.selection.item = configuration.behaviour
  val rule = new ComboBox(SchedulingRule.schedulingRules)
  rule.selection.item = configuration.rule

  // Warrant the coherence in the configuration
  listenTo(minSizePackets.selection, maxSizePackets.selection, behaviour.selection, rule.selection)
  reactions += {
    case SelectionChanged(`behaviour`) =>
      behaviour.selection.item match {
        case Reactive =>
        rule.selection.item = NoRule
        rule.peer.setEditable(false)
        case Proactive =>
          rule.selection.item match {
            case NoRule =>
              rule.selection.item = ECTRule
            case _ =>
          }
          rule.peer.setEditable(true)
      }
    case SelectionChanged(`rule`) =>
      rule.selection.item match {
        case NoRule =>
          behaviour.selection.item = Reactive
          behaviour.peer.setEditable(false)
        case _ =>
          behaviour.selection.item = Proactive
          behaviour.peer.setEditable(true)
      }
    case SelectionChanged(`minSizePackets`) =>
      maxSizePackets.selection.item = Math.max(maxSizePackets.selection.item, minSizePackets.selection.item)
    case SelectionChanged(`maxSizePackets`) =>
      minSizePackets.selection.item = Math.min(maxSizePackets.selection.item, minSizePackets.selection.item)
  }


  // Main panel
  contents = new GridPanel(9, 2) {
    contents += new Label {
      horizontalAlignment = Alignment.Left
      text = "Width"
    }
    contents += width
    contents += new Label {
      horizontalAlignment = Alignment.Left
      text = "Height"
    }
    contents += height
    contents += new Label {
      horizontalAlignment = Alignment.Left
      text = "Number of bodies"
    }
    contents += n
    contents += new Label {
      horizontalAlignment = Alignment.Left
      text = "Number of packets"
    }
    contents += m
    contents += new Label {
      horizontalAlignment = Alignment.Left
      text = "Minimal size of the packets"
    }
    contents += minSizePackets
    contents += new Label {
      horizontalAlignment = Alignment.Left
      text = "Maximal size of the packets"
    }
    contents += maxSizePackets
    contents += new Label {
      horizontalAlignment = Alignment.Left
      text = "Behaviour"
    }
    contents += behaviour
    contents += new Label {
      horizontalAlignment = Alignment.Left
      text = "Rule"
    }
    contents += rule
    contents += Button("Start") {
      start()
    }
    contents += Button("Exit") {
      exit()
    }

  }

  /**
    * Exit with a dialog box
    */
  def exit() {
    val res = Dialog.showConfirmation(contents.head,
      "Are you sure you want to quit ScaSMATA ?",
      optionType = Dialog.Options.YesNo,
      title = title)
    if (res == Dialog.Result.Ok)
      sys.exit(0)
  }

  /**
    * Start a new UI
    */
  def start() {
    val conf = new Configuration(height.selection.item, width.selection.item,
      n.selection.item, m.selection.item,
      minSizePackets.selection.item, maxSizePackets.selection.item,
      behaviour.selection.item, rule.selection.item
    )
    val system = ActorSystem("ScaSMATA") //The Actor system
    system.actorOf(Props(classOf[UI], conf), "UI") //Run simulator
    close()
  }
}

/**
  * Companion object to test the initialization window
  */
object TestInitUI {
  def main(args: Array[String]) {
    val ui = new ConfigurationFrame(new Configuration())
    ui.visible = true
  }
}