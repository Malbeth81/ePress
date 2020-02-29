package com.prosapient.epress

import java.io.File
import java.nio.file.attribute.FileTime
import java.nio.file.{Files, StandardCopyOption}

import org.scalatest._

import scala.compat.Platform
import scala.util.Try

/**
  * Created by Marc-André Lamothe on 28/11/16.
  */
class TemplateManagerSpec extends FlatSpec with Matchers {

  behavior of "TemplateManager"

  val template = new File("src/test/resources/templates/test.html")

  it should "load the template" in {
    TemplateManager.getTemplate(template) should not be empty
  }

  it should "load a copy of the template" in {
    val copy = new File("/tmp/test.html")
    Files.copy(template.toPath, copy.toPath, StandardCopyOption.REPLACE_EXISTING)
    TemplateManager.getTemplate(copy) should not be empty
    TemplateManager.getTemplate(copy) shouldEqual TemplateManager.getTemplate(template)
    TemplateManager.getTemplate(copy).get should not be theSameInstanceAs(TemplateManager.getTemplate(template).get)
  }

  it should "update the loaded template automatically" in {
    val copy = new File("/tmp/test.html")
    Files.copy(template.toPath, copy.toPath, StandardCopyOption.REPLACE_EXISTING)

    val template_v1 = TemplateManager.getTemplate(copy)
    template_v1 should not be empty
    Try(Thread.sleep(1000))
    template_v1.get shouldBe theSameInstanceAs(TemplateManager.getTemplate(copy).get)

    Files.setLastModifiedTime(copy.toPath, FileTime.fromMillis(Platform.currentTime))
    Try(Thread.sleep(1000))

    val template_v2 = TemplateManager.getTemplate(copy)
    template_v2 should not be empty
    template_v2.get should not be theSameInstanceAs(template_v1.get)
  }

  it should "return nothing for an unknown template" in {
    TemplateManager.close()
    TemplateManager.getTemplate(new File("src/test/resources/templates/foobar.html")) shouldBe empty
  }

  it should "remove all templates on close" in {
    TemplateManager.close()
    TemplateManager.getTemplate(template) shouldBe empty
  }

}
