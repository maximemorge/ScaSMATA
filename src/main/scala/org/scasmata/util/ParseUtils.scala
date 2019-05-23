// Copyright (C) Maxime MORGE 2019
package org.scasmata.util

/**
  * Transform a name in an id
  **/
object ParseUtils {
  def name2id(name: String) : Int = {
    val pattern = """\d+""".r
    pattern.findFirstIn(name) match {
      case Some(id)  => id.toInt
      case None =>
        throw new RuntimeException(s"Impossible to extract an id from the name $name")
    }
  }
}