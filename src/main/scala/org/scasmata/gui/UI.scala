// Copyright (C) Maxime MORGE 2018
package org.scasmata.gui

import java.awt.Color

import akka.actor.{Actor, Props}
import akka.util.Timeout
import org.scasmata.environment._
import org.scasmata.simulator._
import org.scasmata.util.{Configuration, Behaviour, Proactive, Reactive, SchedulingRule, NoRule, ECTRule}

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.swing._
import scala.swing.event.{SelectionChanged, ValueChanged}

/**
  * Class representing the user interface
  * @param configuration of the simulation
  */
class UI(var configuration: Configuration) extends Actor {


  val e = new Environment(configuration.height, configuration.width, configuration.n, configuration.m,
    configuration.minSizePackets, configuration.maxSizePackets)
  e.init()

  var delay = 250 //with 250ms of delay
  var simulator = context.actorOf(Props(classOf[Simulator], e, configuration.behaviour, configuration.rule, delay), "Simulator"+Simulator.nextId)//Run simulator

  val mainFrame = new MainFrame(){
    title = "ScaSMATA (Scalable Situated Multi-Agent Task Allocation)"
    val TIMEOUT_VALUE: FiniteDuration = 6000 minutes // Default timeout of a run
    implicit val timeout: Timeout = Timeout(TIMEOUT_VALUE)

    var isRunning = 0 // 0 if never played, 1 if pause, 2 if replayed

    private val boardSquares = Array.ofDim[Label](e.height, e.width)
    // North : button
    private val toolPanel : BoxPanel =
      new BoxPanel(Orientation.Horizontal) {
        // Setup the simulation  based on the current configuration
        val behaviour = new ComboBox(Behaviour.behaviours)
        behaviour.selection.item = configuration.behaviour
        val rule = new ComboBox(SchedulingRule.schedulingRules)
        rule.selection.item = configuration.rule
        // Warrant the coherence in the configuration
        listenTo(behaviour.selection, rule.selection)

        reactions += {
          case SelectionChanged(`behaviour`) =>
            configuration.behaviour = behaviour.selection.item
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
            configuration.rule = rule.selection.item
            rule.selection.item match {
              case NoRule =>
                behaviour.selection.item = Reactive
                behaviour.peer.setEditable(false)
              case _ =>
                behaviour.selection.item = Proactive
                behaviour.peer.setEditable(true)
            }
        }

        contents += Button("Configure") {
          val initUI = new ConfigurationFrame(configuration)
          initUI.visible = true
          close()
        }
        contents += Button("Randomize") {
          e.reGenerate()
          simulator = context.actorOf(Props(classOf[Simulator], e, configuration.behaviour, configuration.rule, delay), "Simulator" + Simulator.nextId) //Run simulator with 250ms of delay
          isRunning = 0
        }
        contents += Button("Reset") {
          e.reset()
          simulator = context.actorOf(Props(classOf[Simulator], e, configuration.behaviour, configuration.rule, delay), "Simulator" + Simulator.nextId) //Run simulator with 250ms of delay
          isRunning = 0
        }
        contents += behaviour

        contents += rule

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

        val next : Button = Button("Next") {
          emit(Next)
        }
        next.enabled = false
        contents += next

        contents += Button("Exit") {
          val res = Dialog.showConfirmation(contents.head,
            "Are you sure you want to quit ScaSMATA ?",
            optionType = Dialog.Options.YesNo,
            title = title)
          if (res == Dialog.Result.Ok)
            sys.exit(0)
        }

        val slider : Slider = new scala.swing.Slider() {
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
            case ValueChanged(_) =>
              delay = value
              simulator ! Delay(delay)
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
  def emit(msg : ManagingMessage) : Unit = simulator ! msg

  /**
    * Message handling
    **/
  def receive : PartialFunction[Any,Unit] = {
    case Outcome(steps) =>
      var result = ""
      steps.foreach{
        case(id,nbSteps) => result+=s"Agent$id : $nbSteps steps\n"
      }
      result+="Max= "+steps.values.max+"\n"
      //result+="Mean= "+steps.values.sum.toDouble/steps.values.size+"\n"
      Dialog.showMessage(mainFrame,result, title="OK")

    case msg@_ =>
      println("Simulator: it receives a message which was not expected: " + msg)
  }
}
