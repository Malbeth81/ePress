package com.prosapient.epress

import java.io.File

import org.scalatest._

import scala.compat.Platform

/**
  * Created by Marc-André Lamothe on 28/11/16.
  */
class TemplateFactorySpec extends FlatSpec with Matchers {

  behavior of "TemplateFactory"

  val file = new File("src/test/resources/templates/test.html")

  it should "load the template in less than 50 milliseconds" in {
    val start = Platform.currentTime
    val template = TemplateFactory.loadFromFile(file)
    val time = Platform.currentTime - start
    info(s"Time: $time milliseconds")

    template should not be empty
    assert(time < 50)
  }

  it should "return nothing for an unknown template" in {
    val template = TemplateFactory.loadFromFile(new File("src/test/resources/templates/foobar.html"))
    template shouldBe empty
  }

  it should "render the template in less than 15 milliseconds" in {
    val template = TemplateFactory.loadFromFile(file)

    template should not be empty

    val start = Platform.currentTime
    template.get.toString(Map[String, String](
      "name" -> "John"
    ))
    val time = Platform.currentTime - start
    info(s"Time: $time milliseconds")
    assert(time < 50)
  }

  it should "render 10000 copy of the template in under 2 seconds" in {
    val template = TemplateFactory.loadFromFile(file)

    template should not be empty

    val start = Platform.currentTime
    for (i <- 0 until 10000) {
      template.get.toString(Map[String, String](
        "name" -> "John"
      ))
    }
    val time = Platform.currentTime - start
    info(s"Time: $time milliseconds")
    assert(time < 2000)
  }

}
