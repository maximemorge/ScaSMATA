// Copyright (C) Maxime MORGE 2018
package org.scasmata.util

import org.scasmata.gui.ConfigurationFrame

import java.util.Locale

/**
  * Main application
  */
object Main{
  def main(args: Array[String]): Unit = {
    Locale.setDefault(new Locale("en", "GB"))
    val ui = new ConfigurationFrame(new Configuration())
    ui.visible = true
  }
}
