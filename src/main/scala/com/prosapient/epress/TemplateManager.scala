package com.prosapient.epress

import java.io.File
import java.util.concurrent.atomic.AtomicBoolean

import scala.collection.mutable
import scala.util.Try

/**
  * Created by Marc-André Lamothe on 28/11/16.
  */
object TemplateManager {
  private val closed = new AtomicBoolean(false)

  private val templates = mutable.Map[File, (Long, Template)]()

  private val monitor = new Runnable {
    override def run(): Unit = {
      while (!closed.get) {
        val files = mutable.Buffer[File]()

        // Get list of updated templates
        templates synchronized {
          for ((file, (time, _)) <- templates) {
            if (file.lastModified > time)
              files += file
          }
        }

        // Re-load updated templates
        files.foreach(loadTemplate)

        Try(Thread.sleep(100))
      }
    }
  }
  new Thread(monitor).start()

  def close(): Unit = {
    closed.set(true)
    templates synchronized templates.clear
  }

  def getTemplate(file: File): Option[Template] = {
    if (!closed.get) {
      templates synchronized templates.get(file) match {
        case Some((_, result: Template)) =>
          Some(result)
        case _ =>
          loadTemplate(file)
      }
    }
    else
      None
  }

  private def loadTemplate(file: File): Option[Template] = {
    val result = TemplateFactory.loadFromFile(file)
    if (result.isDefined)
      templates synchronized templates.put(file, (file.lastModified, result.get))
    result
  }

}
