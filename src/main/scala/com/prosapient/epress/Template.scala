package com.prosapient.epress

/**
  * Created by Marc-André Lamothe on 28/11/16.
  */
trait Template {
  def toArray(data: Map[String, String]): Array[Char]
  def toString(data: Map[String, String]): String
}
