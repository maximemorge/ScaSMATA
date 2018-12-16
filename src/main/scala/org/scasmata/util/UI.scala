// Copyright (C) Maxime MORGE 2018
package org.scasmata.util

import scala.swing._
import java.awt.Color

import scala.swing.event.ValueChanged
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.util.Timeout

import scala.language.postfixOps
import scala.concurrent.duration._
import akka.pattern.ask

import scala.concurrent.Await
import org.scasmata.environment._
import org.scasmata.actor.{Pause, Play, Replay, Next, Simulator}

/**
  * Class representing the user interface
  * @param e where agent interacts
  */
class UI(val e: Environment) extends MainFrame {
  title = "ScaSMATA (Scalable Situated Multi-Agent Task Allocation) GUI"

  val system = ActorSystem("ScaSMATASolver") //The Actor system
  var simulator =  system.actorOf(Props(classOf[Simulator], e, 250), "Simulator"+Simulator.nextId)//Run simulator with 250ms of delay

  val TIMEOUTVALUE : FiniteDuration = 6000 minutes // Default timeout of a run
  implicit val timeout : Timeout = Timeout(TIMEOUTVALUE)

  var isRunning = 0 // 0 if never played, 1 if pause, 2 if replayed

  private val boardSquares = Array.ofDim[Label](e.height, e.width)
  // North : button
  private val toolPanel = new BoxPanel(Orientation.Horizontal) {
    contents += Button("New"){
      e.reinit()
      simulator = system.actorOf(Props(classOf[Simulator], e, 250), "Simulator"+Simulator.nextId)//Run simulator with 250ms of delay
      isRunning = 0
    }

    contents += Button("Play/Pause") {
      isRunning match {
        case 0 =>
          isRunning = 1
          simulator ! Play
        case 1 =>
          isRunning = 2
          next.enabled = true
          simulator ! Pause
        case 2 =>
          isRunning = 1
          next.enabled = false
          simulator ! Replay
      }
    }

    val next= Button("Next"){
      simulator ! Next
    }
    next.enabled = false
    contents += next

    contents += Button("Exit"){
      sys.exit(0)
    }
    for (e <- contents) e.xLayoutAlignment = 0.0
      border = Swing.EmptyBorder(10, 10, 10, 10)
  }
  // Center : grid panel
  private val gridPanel=  new GridPanel(e.height, e.height) {
    background = Color.WHITE
  }
  // Draw grid panel
  for (i <- 0 until e.height; j <- 0 until e.width) {
    boardSquares(i)(j) = e.get(i,j).label()
    boardSquares(i)(j).listenTo(e.get(i,j))
    boardSquares(i)(j).reactions += {
      case ValueChanged(label: Label) =>
        boardSquares(i)(j).icon= label.icon
        boardSquares(i)(j).repaint()
    }
    gridPanel.contents += boardSquares(i)(j)
  }
  // Main panel
  contents = new BorderPanel {
    layout(toolPanel) = scala.swing.BorderPanel.Position.North
    layout(gridPanel) = scala.swing.BorderPanel.Position.Center
  }
}
