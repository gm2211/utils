package com.gm2211.serialization

import java.io.{ByteArrayInputStream, File, InputStreamReader}
import java.nio.file.Path
import java.util

import com.gm2211.collections.MapExtensions._

import scala.collection.JavaConverters._
import scala.io.Source


object Yaml {
  import org.yaml.snakeyaml.Yaml
  private val yaml = new Yaml

  def readValueAsMap(string: String): Map[String, Any] =
    readAsMap(new InputStreamReader(new ByteArrayInputStream(string.getBytes)))

  def readResourceAsMap(resourceName: String)(implicit clazz: Class[_]): Map[String, Any] =
    readAsMap(Source.fromResource(resourceName, clazz.getClassLoader).reader())

  def readPathAsMap(path: Path): Map[String, Any] = readFileAsMap(path.toFile)
  def readFileAsMap(file: File): Map[String, Any] = readAsMap(Source.fromFile(file).reader())
  def readAsMap(reader: InputStreamReader): Map[String, Any] = {
    val map: Map[String, Any] = yaml.load[util.HashMap[String, Any]](reader)
      .asScala
      .toMap

    map.deepConvert { case m: java.util.HashMap[_, _] => m.asScala.toMap }
  }
}
