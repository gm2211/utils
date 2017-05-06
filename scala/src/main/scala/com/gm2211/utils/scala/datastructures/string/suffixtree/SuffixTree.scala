package com.gm2211.utils.scala.datastructures.string.suffixtree
import scala.collection.SeqView

trait SuffixTree {
  import scala.language.implicitConversions
  implicit def toView(string: String): SeqView[Char, String] = string.view

  /**
   * Finds the provided string as an exact suffix in this tree.
   * <p>
   * Example:
   * <p>
   * Given a suffix tree for "abbc"
   * <p>
   * "bb" will not match
   * <br>
   * "bbc" will
   *
   * @param string The string to be found
   * @return A list of start indices where the provided string can be found within the original string
   */
  def find(string: SeqView[Char, String]): List[Int]

  /**
   * Finds the provided string as a substring of any of the suffixes in this tree.
   *
   * @param string The string to be found
   * @return A list of start indices where the provided string can be found within the original string
   */
  def findSubstring(string: SeqView[Char, String]): List[Int]

  /**
   * Finds the longest prefix of the provided string as a substring of any of the suffixes in this tree.
   * <p>
   * Example:
   * <p>
   * Given a tree for "abcde"
   * <p>
   * And input: "bcdzzz"
   * <p>
   * This method will return (3, List(1)), since "bcd" is a prefix of the input and a substring of suffix "bcde" which
   * starts at index 1 withing "abcde"
   *
   * @param stringView A view of the string for which to find the longest prefix. Using a view is convenient for
   * algorithms where you build a suffix tree as you go and try to find substrings so far.
   * Example:
   * {{{ suffixTreeSoFar.findLongestSubstringPrefixOf(string.view(curIdx, curIdx + lookAheadLen)) }}}
   * @return
   */
  def findLongestSubstringPrefixOf(stringView: SeqView[Char, String]): Option[(Int, List[Int])]
}
