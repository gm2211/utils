package com.gm2211.utils.scala.datastructures.string.suffixtree

import org.scalatest.{FlatSpec, Matchers}

class SuffixTreeTest extends FlatSpec with Matchers {

  "A Suffix Tree" should "take into account repeated letters" in {
    val tree = MutableSuffixTree.build("abbc")
    tree.findSubstring("abc") shouldBe empty
  }

  "A Suffix Tree" should "find all substrings" in {
    val tree = MutableSuffixTree.build("abbc")
    tree.findSubstring("a") shouldBe List(0)
    tree.findSubstring("b") shouldBe List(1, 2)
    tree.findSubstring("ab") shouldBe List(0)
    tree.findSubstring("abb") shouldBe List(0)
    tree.findSubstring("abbc") shouldBe List(0)
    tree.findSubstring("bbc") shouldBe List(1)
    tree.findSubstring("bc") shouldBe List(2)
    tree.findSubstring("c") shouldBe List(3)
    tree.findSubstring("bb") shouldBe List(1)
  }

  "A Suffix Tree" should "find repeated substrings" in {
    val tree = MutableSuffixTree.build("abbcdizbbcdi")
    tree.findSubstring("bbcdi") shouldBe List(1, 7)
    tree.findSubstring("abbcdi") shouldBe List(0)
  }
}
