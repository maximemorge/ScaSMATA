// Copyright (C) Maxime MORGE 2018
package org.scasmata.util

import scala.swing._
import java.awt.Color
import scala.swing.BorderPanel.Position._
import scala.swing.event.ValueChanged

import org.scasmata.environment._

/**
  * Class representing the user interface
  * @param environment
  */
class UI(val environment: Environment) extends MainFrame {
  title = "ScaSMATA (Scalable Situated Multi-Agent Task Allocation) GUI"
  private val boardSquares = Array.ofDim[Label](environment.height, environment.width)
  // North
  private val toolPanel = new BoxPanel(Orientation.Horizontal) {
      contents += Button("New"){ environment.init() }
      contents += Button("Exit"){ sys.exit(0) }
      for (e <- contents) e.xLayoutAlignment = 0.0
      border = Swing.EmptyBorder(10, 10, 10, 10)
    }
  // Center
  private val gridPanel=  new GridPanel(environment.height, environment.height) {
    background = Color.WHITE
  }
  // Draw gridPanel
  for (i <- 0 until environment.height; j <- 0 until environment.width) {
    boardSquares(i)(j) = environment.get(i,j).label()
    boardSquares(i)(j).listenTo(environment.get(i,j))
    boardSquares(i)(j).reactions += {
      case ValueChanged(label: Label) =>
        boardSquares(i)(j).icon= label.icon
        boardSquares(i)(j).repaint()
    }
    gridPanel.contents += boardSquares(i)(j)
  }
  // Main panel
  contents = new BorderPanel {
    layout(toolPanel) = North
    layout(gridPanel) = Center
  }
}
