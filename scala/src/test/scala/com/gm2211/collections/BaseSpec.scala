package com.gm2211.collections

import java.io.File
import java.net.URL

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

trait BaseSpec extends FlatSpec with Matchers {
  implicit val testClass: Class[_] = getClass

  def resourceUrl(resourceName: String): URL = testClass.getClassLoader.getResource(resourceName)
  def resource(resourceName: String): File = new File(resourceUrl("template-properties.yml").getFile)
  def readResource(resourceName: String) = Source.fromURL(resourceUrl(resourceName)).getLines().mkString("\n")
}
