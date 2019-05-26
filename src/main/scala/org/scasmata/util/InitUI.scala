// Copyright (C) Maxime MORGE 2019
package org.scasmata.util

import scala.swing._
import akka.actor.{ActorSystem, Props}

import org.scasmata.environment.Environment

/**
  * Class representing the initialization window
  */
class InitUI extends MainFrame {

  title = "ScaSMATA setup"

  // Setup the environment
  val width = new ComboBox(List(16, 14, 12, 10, 8, 6, 4))
  val height = new ComboBox(List(8, 7, 6, 5, 4))
  val n = new ComboBox(List(4, 3, 2, 1))
  val m = new ComboBox(List(8, 7, 6, 5, 4, 3, 2, 1))
  val minSizePackets = new ComboBox(List(1, 2, 3, 4))
  val maxSizePackets = new ComboBox(List(1, 2, 3, 4))

  // Setup the simulation
  val operationalAgent = new ComboBox(List("ProactiveOperationalAgent"))//,"ReactiveOperationalAgent"))
  val solver = new ComboBox(List("ECTSolver"))//, "RandomSolver", "ExhaustiveSolver", "DealSolver"))

  // Main panel
  contents = new GridPanel(9,2){
    contents += new Label{
      horizontalAlignment = Alignment.Left
      text = "Width"
    }
    contents += width
    contents += new Label{
      horizontalAlignment = Alignment.Left
      text = "Height"
    }
    contents += height
    contents += new Label{
      horizontalAlignment = Alignment.Left
      text = "Number of bodies"
    }
    contents += n
    contents += new Label{
      horizontalAlignment = Alignment.Left
      text = "Number of packets"
    }
    contents += m
    contents += new Label{
      horizontalAlignment = Alignment.Left
      text = "Minimal size of the packets"
    }
    contents += minSizePackets
    contents += new Label{
      horizontalAlignment = Alignment.Left
      text = "Maximal size of the packets"
    }
    contents += maxSizePackets
    contents += new Label{
      horizontalAlignment = Alignment.Left
      text = "OperationalAgent"
    }
    contents += operationalAgent
    contents += new Label{
      horizontalAlignment = Alignment.Left
      text = "Solver"
    }
    contents += solver
    contents += Button("Start") {
      start()
    }
    contents += Button("Close") { exit()
    }
  }

  /**
    * Start a new UI
    */
  def start() {
    val e = new Environment(height.selection.item, width.selection.item,
      n.selection.item, m.selection.item,
      minSizePackets.selection.item, maxSizePackets.selection.item)
    e.init()
    val system = ActorSystem("ScaSMATASolver") //The Actor system
    system.actorOf(Props(classOf[UI], e), "UI")//Run simulator
  }

  /**
    * Exit with a dialog box
    */
  def exit() {
    val res = Dialog.showConfirmation(contents.head,
      "Are you sure you want to quit ScaSMATA ?",
      optionType=Dialog.Options.YesNo,
      title=title)
    if (res == Dialog.Result.Ok)
      sys.exit(0)
  }
}


/**
  * Companion object to test the initialization window
  */
object TestInitUI {
  def main(args: Array[String]) {
    val ui = new InitUI
    ui.visible = true
  }
}