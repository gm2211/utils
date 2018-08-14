package com.gm2211.collections

import org.scalatest.{FlatSpec, Matchers}

class MapExtensionsTest extends FlatSpec with Matchers {

  import MapExtensions._

  "The merged map" should "contain values from the right map for common keys" in {
    val left = Map("common" -> "original")
    val right = Map("common" -> "override")

    val mergedMapRecursive = (left, right).recursiveMergeMaps()
    val mergedMapIterative = (left, right).recursiveIterativeMergeMaps()

    mergedMapRecursive shouldEqual mergedMapIterative

    mergedMapRecursive should have size 1
    mergedMapRecursive should contain("common" -> "override")
  }

  it should "contain non-common (key,value)s from both maps" in {
    val left = Map(
      "left" -> "yes",
      "common" -> "original"
    )
    val right = Map(
      "right" -> "also-yes",
      "common" -> "override"
    )

    val mergedMapRecursive = (left, right).recursiveMergeMaps()
    val mergedMapIterative = (left, right).recursiveIterativeMergeMaps()

    mergedMapRecursive shouldEqual mergedMapIterative

    mergedMapRecursive should have size 3
    mergedMapRecursive should contain("common" -> "override")
    mergedMapRecursive should contain("left" -> "yes")
    mergedMapRecursive should contain("right" -> "also-yes")
  }

  it should "merge nested maps" in {
    val left = Map(
      "common-depth-0" -> "original-depth-0",
      "nested" -> Map[String, String](
        "left" -> "yes",
        "common-depth-1" -> "original-depth-1"
      )
    )
    val right = Map(
      "common-depth-0" -> "override-depth-0",
      "nested" -> Map[String, String](
        "right" -> "also-yes",
        "common-depth-1" -> "override-depth-1"
      )
    )

    val mergedMapRecursive = (left, right).recursiveMergeMaps()
    val mergedMapIterative = (left, right).recursiveIterativeMergeMaps()

    mergedMapRecursive shouldEqual mergedMapIterative

    mergedMapRecursive should have size 2
    mergedMapRecursive should contain("common-depth-0" -> "override-depth-0")
    mergedMapRecursive should contain key "nested"

    val mergedNestedMap = mergedMapRecursive("nested").asInstanceOf[Map[String, String]]

    mergedNestedMap should have size 3
    mergedNestedMap should contain("left" -> "yes")
    mergedNestedMap should contain("right" -> "also-yes")
    mergedNestedMap should contain("common-depth-1" -> "override-depth-1")

  }

  it should "pass a smoke test" in {
    val left: Map[String, Any] = Map[String, Any](
      "common-0-depth-0" -> 1,
      "left" -> 10,
      "common-map-0-depth-0" -> Map[String, Any](
        "common-map-0-depth-1" -> Map[String, Any](
          "common-letter-depth-2" -> 'c',
          "left" -> 2,
          "common-map-0-depth-2" -> Map[String, Any](
            "common-article-depth-2" -> "the",
            "left-map-depth-3" -> Map[String, Any](
              "left" -> "ciao"
            )
          )
        ),
        "number" -> 10,
        "common-map-1-depth-1" -> Map[String, Any](
          "common-lang-depth-2" -> "it"
        )
      ),
      "common-list-depth-0" -> List(1, 2)
    )

    val right: Map[String, Any] = Map(
      "common-0-depth-0" -> 2,
      "common-list-depth-0" -> List(3, 4),
      "right" -> 20,
      "common-map-0-depth-0" -> Map[String, Any](
        "common-map-0-depth-1" -> Map[String, Any](
          "common-letter-depth-2" -> 'z',
          "right" -> 5,
          "common-map-0-depth-2" -> Map[String, Any](
            "common-article-depth-2" -> "a",
            "right-map-depth-3" -> Map[String, Any](
              "right" -> "see ya"
            )
          )
        ),
        "number" -> 15,
        "common-map-1-depth-1" -> Map[String, Any](
          "common-lang-depth-2" -> "en"
        )
      )
    )

    val expected: Map[String, Any] = Map(
      "common-0-depth-0" -> 2,
      "common-list-depth-0" -> List(1, 2, 3, 4),
      "right" -> 20,
      "left" -> 10,
      "common-map-0-depth-0" -> Map[String, Any](
        "common-map-0-depth-1" -> Map[String, Any](
          "common-letter-depth-2" -> 'z',
          "right" -> 5,
          "left" -> 2,
          "common-map-0-depth-2" -> Map[String, Any](
            "common-article-depth-2" -> "a",
            "left-map-depth-3" -> Map[String, Any](
              "left" -> "ciao"
            ),
            "right-map-depth-3" -> Map[String, Any](
              "right" -> "see ya"
            )
          )
        ),
        "number" -> 15,
        "common-map-1-depth-1" -> Map[String, Any](
          "common-lang-depth-2" -> "en"
        )
      )
    )

    val mergedRecursively = (left, right).recursiveMergeMaps()
    val mergedIteratively = (left, right).recursiveIterativeMergeMaps()

    mergedIteratively shouldEqual mergedRecursively
    mergedIteratively shouldEqual expected
  }
}
