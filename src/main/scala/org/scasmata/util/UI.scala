// Copyright (C) Maxime MORGE 2018
package org.scasmata.util

import scala.swing._
import java.awt.Color
import scala.swing.event.ValueChanged

import akka.actor.ActorRef
import akka.util.Timeout
import scala.language.postfixOps
import scala.concurrent.duration._
import akka.pattern.ask
import scala.concurrent.Await

import org.scasmata.environment._
import org.scasmata.actor.{Next, Outcome, Pause, Play}

/**
  * Class representing the user interface
  * @param e where agent interacts
  */
class UI(val e: Environment, val simulator: ActorRef) extends MainFrame {
  title = "ScaSMATA (Scalable Situated Multi-Agent Task Allocation) GUI"

  val TIMEOUTVALUE : FiniteDuration = 6000 minutes // Default timeout of a run
  implicit val timeout : Timeout = Timeout(TIMEOUTVALUE)

  var isRunning = false

  private val boardSquares = Array.ofDim[Label](e.height, e.width)
  // North : button
  private val toolPanel = new BoxPanel(Orientation.Horizontal) {
    contents += Button("New"){
      e.reinit()
    }
    contents += Button("Play/Pause") {
      if (!isRunning) {
        isRunning = true
        simulator ! Play
      } else {
        isRunning = false
        simulator ! Pause
      }
    }

    contents += Button("Next"){
      simulator ! Next
    }
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
