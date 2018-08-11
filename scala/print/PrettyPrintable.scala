implicit class PrettyPrintable(obj: Any) {

  def prettyPrint(depth: Int = 0): String = {
    val stringBuilder = new StringBuffer()
    val tabs = "\t" * depth

    obj match {
      case map: Map[_, _] =>
        stringBuilder.append(s"{\n")
        stringBuilder.append(
          map.map { case (key, value) => s"$tabs\t$key -> ${value.prettyPrint(depth + 1)}" }.mkString(s",\n")
        )
        stringBuilder.append(s"\n$tabs}")

      case list: List[_] =>
        stringBuilder.append(s"[\n")
        stringBuilder.append(
          list.map(value => s"$tabs\t${value.prettyPrint(depth + 1)}").mkString(s",\n")
        )
        stringBuilder.append(s"\n$tabs]")
      case _ =>
        stringBuilder.append(s"$obj")
    }

    stringBuilder.toString
  }
}
