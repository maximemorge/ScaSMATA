// Copyright (C) Maxime MORGE 2018
package org.scasmata.util

import scala.swing._
import java.awt.Color

import scala.swing.event.ValueChanged
import akka.actor.{Actor, ActorSystem, Props}
import akka.util.Timeout

import scala.language.postfixOps
import scala.concurrent.duration._
import akka.pattern.ask
import javafx.scene.control.Slider

import scala.concurrent.Await
import org.scasmata.environment._
import org.scasmata.actor._

/**
  * Class representing the user interface
  * @param e where agent interacts
  */
class UI(val e: Environment) extends Actor {

  var delay = 250 //with 250ms of delay
  var simulator = context.actorOf(Props(classOf[Simulator], e, delay), "Simulator"+Simulator.nextId)//Run simulator

  val mainFrame = new MainFrame(){
    title = "ScaSMATA (Scalable Situated Multi-Agent Task Allocation) GUI"
    val TIMEOUTVALUE: FiniteDuration = 6000 minutes // Default timeout of a run
    implicit val timeout: Timeout = Timeout(TIMEOUTVALUE)

    var isRunning = 0 // 0 if never played, 1 if pause, 2 if replayed
    private val boardSquares =
    Array.ofDim[Label](e.height, e.width)
    // North : button
    private val toolPanel =
    new BoxPanel(Orientation.Horizontal) {
      contents += Button("New") {
        e.reinit()
        simulator = context.actorOf(Props(classOf[Simulator], e, delay), "Simulator" + Simulator.nextId) //Run simulator with 250ms of delay
        isRunning = 0
      }
      contents += Button("Play/Pause") {
        isRunning match {
          case 0 =>
            isRunning = 1
            emit(Play)
          case 1 =>
            isRunning = 2
            next.enabled = true
            emit(Pause)
          case 2 =>
            isRunning = 1
            next.enabled = false
            emit(Replay)
        }
      }

      val next = Button("Next") {
        emit(Next)
      }
      next.enabled = false
      contents += next

      contents += Button("Exit") {
        sys.exit(0)
      }

      val slider = new scala.swing.Slider() {
        name = delay.toString
        min = 0
        max = 1000
        majorTickSpacing = 100
        minorTickSpacing = 25
        paintTicks = true
        value = delay
        labels = Map[Int, Label](0 -> new Label("Fast"), 500 -> new Label("Medium"), 1000 -> new Label("Slow"))
        paintLabels = true
        reactions += {
          case ValueChanged(_) => delay = value
        }
      }
      contents += slider
      for (e <- contents) e.xLayoutAlignment = 0.0
      border = Swing.EmptyBorder(10, 10, 10, 10)
    }
    // Center : grid panel
    private val gridPanel =
    new GridPanel(e.height, e.height) {
      background = Color.WHITE
    }
    // Draw grid panel
    for (i <- 0 until e.height; j <- 0 until e.width) {
      boardSquares(i)(j) = e.get(i, j).label()
      boardSquares(i)(j).listenTo(e.get(i, j))
      boardSquares(i)(j).reactions += {
        case ValueChanged(label: Label) =>
          boardSquares(i)(j).icon = label.icon
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
  mainFrame.visible = true

  /**
    * Play the simulator
    */
  def emit(msg : ManagingMessage) = simulator ! msg
  /**
    * Message handling
    **/
  def receive = {
    case Outcome(steps) =>
      var result = ""
      steps.foreach{
        case(id,nbSteps) => result+=s"Agent$id : $nbSteps steps\n"
      }
      result+="Max= "+steps.values.max+"\n"
      result+="Mean= "+steps.values.sum/steps.values.size+"\n"
      Dialog.showMessage(mainFrame,result, title="OK")

    case msg@_ =>
      println("Simulator: it receives a message which was not expected: " + msg)

  }
}
