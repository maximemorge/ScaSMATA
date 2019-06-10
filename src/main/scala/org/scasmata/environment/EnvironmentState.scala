// Copyright (C) Maxime MORGE 2019
package org.scasmata.environment

/**
  * Environment state
  * @param height of the grid
  * @param width of the grid
  */
class EnvironmentState(val height: Int, val width: Int){

  //Create the grid and the maps of packets/bodies/teams
  val grid = Array.ofDim[Cell](height, width)
  for (i <- 0 until height; j <- 0 until width)
    grid(i)(j) = new Cell(i,j)
  var packets : Map[Int,Packet] = Map[Int,Packet]()
  var bodies : Map[Int,Body] = Map[Int,Body]()
  var teams : Map[Int,Team]= Map[Int,Team]()

  /**
    * Save the environnment state, i.e.
    * @param g the current grid
    * @param p the packets
    * @param b the bodies
    * @param t the teams
    */
  def save(g: Array[Array[Cell]], p:  Map[Int,Packet], b : Map[Int,Body], t : Map[Int,Team]) : Unit = {
    for (i <- 0 until height; j <- 0 until width) {
      if (g(i)(j).isEmpty) grid(i)(j).setContent(None)
      else {
        val c : Entity = g(i)(j).content.get
        grid(i)(j).setContent(Some(c.copy()))
      }
    }
    packets = p
    bodies = b
    teams = t
  }

}
