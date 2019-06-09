// Copyright (C) Maxime MORGE 2018
package org.scasmata.util

import org.scasmata.gui.ConfigurationFrame

/**
  * Main application
  */
object Main{
  def main(args: Array[String]): Unit = {
    val ui = new ConfigurationFrame(new Configuration())
    ui.visible = true
  }
}
