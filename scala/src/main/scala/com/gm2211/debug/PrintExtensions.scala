package com.gm2211.debug

import scala.collection.mutable

object PrintExtensions {

  implicit class PrettyPrintable(obj: Any) {

    def prettyPrint(depth: Int = 0): String = {
      val stringBuilder = new StringBuilder()

      prettyPrintHelper(depth, stringBuilder)

      stringBuilder.result()
    }

    private def prettyPrintHelper(depth: Int, stringBuilder: StringBuilder): Unit = {
      val tabs = "\t" * depth

      obj match {
        case map: mutable.Map[_, _] => prettyPrintMapHelper(depth, stringBuilder, tabs, map.toMap)
        case map: Map[_, _] => prettyPrintMapHelper(depth, stringBuilder, tabs, map)
        case list: List[_] =>
          stringBuilder.append(s"[\n")
          stringBuilder.append(
            list.map(value => s"$tabs\t${value.prettyPrint(depth + 1)}").mkString(s",\n")
          )
          stringBuilder.append(s"\n$tabs]")
        case _ =>
          stringBuilder.append(s"$obj")
      }
    }

    private def prettyPrintMapHelper(
      depth: Int,
      stringBuilder: StringBuilder,
      tabs: String,
      map: Map[_, _]
    ): Unit = {
      stringBuilder.append(s"{\n")
      stringBuilder.append(
        map.map { case (key, value) => s"$tabs\t$key -> ${value.prettyPrint(depth + 1)}" }.mkString(s",\n")
      )
      stringBuilder.append(s"\n$tabs}")
    }
  }

}
