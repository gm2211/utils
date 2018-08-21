package com.gm2211.collections

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

final class MapExtensionsSpec extends BaseSpec {
  import MapExtensions._

  "The merged map" should "contain values from the right map for common keys" in {
    val left = Map("common" -> "original")
    val right = Map("common" -> "override")

    val mergedMapRecursive = (left, right).deepMergeRecursive()
    val mergedMapIterative = (left, right).deepMergeIterative()

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

    val mergedMapRecursive = (left, right).deepMergeRecursive()
    val mergedMapIterative = (left, right).deepMergeIterative()

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

    val mergedMapRecursive = (left, right).deepMergeRecursive()
    val mergedMapIterative = (left, right).deepMergeIterative()

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

    val mergedRecursively = (left, right).deepMergeRecursive()
    val mergedIteratively = (left, right).deepMergeIterative()

    mergedIteratively shouldEqual mergedRecursively
    mergedIteratively shouldEqual expected
  }

  "Specifying a map output type" should "convert all nested maps" in {
    val withImmutableNestedMaps = map.deepConvert[Map[String, Any]] { case x: Any => x }

    withImmutableNestedMaps shouldBe a [Map[_, _]]
    withImmutableNestedMaps("two") shouldBe a [Map[_, _]]
    withImmutableNestedMaps("two")("three") shouldBe a [Map[_, _]]
    withImmutableNestedMaps("two")("three")("five") shouldBe a [Map[_, _]]
    withImmutableNestedMaps("two")("seven") shouldBe a [Map[_, _]]
    withImmutableNestedMaps("two")("seven")("nine") shouldBe a [Map[_, _]]
  }

  "Deep converting a map" should "convert all nested values but leave maps intact" in {
    val withIntsDoubled = map.deepConvert[mutable.Map[String, Any]] { case i: Number => i.intValue() * 2 }

    validateInputMap()

    withIntsDoubled shouldBe a [mutable.Map[_, _]]
    withIntsDoubled("one") should equal (2)
    withIntsDoubled("two") shouldBe a [mutable.Map[_, _]]
    withIntsDoubled("two")("three") shouldBe a [mutable.Map[_, _]]
    withIntsDoubled("two")("three")("four") should equal(8)
    withIntsDoubled("two")("three")("five") shouldBe a [mutable.Map[_, _]]
    withIntsDoubled("two")("three")("five")("six") should equal(12)
    withIntsDoubled("eleven") should equal (22)
    withIntsDoubled("two")("seven") shouldBe a [mutable.Map[_, _]]
    withIntsDoubled("two")("seven")("eight") should equal(16)
    withIntsDoubled("two")("seven")("nine") shouldBe a [mutable.Map[_, _]]
    withIntsDoubled("two")("seven")("nine")("ten") should equal(20)
  }

  "Two stacks" should "achieve post-order traversal" in {
    val stack1 = Stack[TreeNode]()
    val stack2 = Stack[TreeNode]()

    stack1.push(tree)

    while (stack1.nonEmpty) {
      val node = stack1.pop()

      stack2.push(node)

      node match {
        case n: Node => n.children.foreach(stack1.push)
        case _ => // do nothing
      }
    }

    val result = ListBuffer[Int]()

    while (stack2.nonEmpty) {
      result.append(stack2.pop().value)
    }

    result.toList should contain inOrder (5, 6, 2, 7, 8, 9, 3, 10, 11, 12, 4, 1)
  }

  "One stack" should "achieve post-order traversal if Node is mutable" in {
    var curNode: Option[TreeNode] = Some(tree)
    val stack = Stack[TreeNode]()
    val result = ListBuffer[Int]()

    do {
      while (curNode.isDefined) {
        curNode.get match {
          case n: Node if n.children.size > 1 =>
            n.children.tail.reverseIterator.foreach(stack.push)
            stack.push(n)
            curNode = Some(n.children.head)
          case n: Node if n.children.size == 1 =>
            stack.push(n)
            curNode = Some(n.children.head)
          case leaf: Leaf =>
            stack.push(leaf)
            curNode = None
          case _ =>
            curNode = None
        }
      }

      if (stack.nonEmpty) {
        val node = stack.pop()

        node match {
          case n: Node if n.children.size > 1 && stack.nonEmpty && n.children.tail.head.equals(stack.peek()) =>
            val right = stack.pop()
            n.children.remove(1)
            stack.push(node)
            curNode = Some(right)
          case _ =>
            result.append(node.value)
            curNode = None
        }
      }

    } while (stack.nonEmpty)

    result.toList should contain inOrder (5, 6, 2, 7, 8, 9, 3, 10, 11, 12, 4, 1)
  }

  sealed trait TreeNode {
    val value: Int

    override def toString: String = value.toString
  }

  case class Node(value: Int, children: mutable.ArrayBuffer[TreeNode]) extends TreeNode
  case class Leaf(value: Int) extends TreeNode

  val tree = Node(
    1,
    mutable.ArrayBuffer(
      Node(2, mutable.ArrayBuffer(Leaf(5), Leaf(6))),
      Node(3, mutable.ArrayBuffer(Leaf(7), Leaf(8), Leaf(9))),
      Node(4, mutable.ArrayBuffer(Leaf(10), Leaf(11), Leaf(12)))
    )
  )

  val map: Map[String, Any] = Map(
    "one" -> 1,
    "two" -> mutable.Map(
      "three" -> mutable.Map(
        "four" -> 4,
        "five" -> mutable.Map(
          "six" -> 6
        )
      ),
      "seven" -> mutable.Map(
        "eight" -> 8,
        "nine" -> mutable.Map(
          "ten" -> 10
        )
      )
    ),
    "eleven" -> 11
  )

  private def validateInputMap(): Unit = {
    map should contain("one" -> 1)
    map should contain("eleven" -> 11)

    val nestedMapTwo = map("two").asInstanceOf[mutable.Map[String, Any]]

    nestedMapTwo should contain key "three"
    nestedMapTwo should contain key "seven"

    val nestedMapThree = nestedMapTwo("three").asInstanceOf[mutable.Map[String, Any]]
    val nestedMapSeven = nestedMapTwo("seven").asInstanceOf[mutable.Map[String, Any]]

    nestedMapThree should contain("four" -> 4)
    nestedMapThree should contain("five" -> mutable.Map("six" -> 6))
    nestedMapSeven should contain("eight" -> 8)
    nestedMapSeven should contain("nine" -> mutable.Map("ten" -> 10))
  }

  implicit def anyToMap(x: Any): scala.collection.Map[Any, Any] = x match {
    case _: scala.collection.Map[_, _] => x.asInstanceOf[scala.collection.Map[Any, Any]]
  }
}
