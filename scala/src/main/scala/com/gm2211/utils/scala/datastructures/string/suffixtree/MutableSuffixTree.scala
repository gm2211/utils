package com.gm2211.utils.scala.datastructures.string.suffixtree

import java.util.concurrent.atomic.AtomicInteger
import scala.util.control.Breaks.{breakable, break}
import scala.collection.{SeqView, mutable}


object MutableSuffixTree {
  def build(string: String): MutableSuffixTree = {
    string.foldLeft(new MutableSuffixTree()) {
      case (tree, char) => tree.addChar(char)
    }
  }
}

/**
 * A suffix tree to which it's possible to append new characters. Only supports strings up to length 2&#94;32 since it
 * uses integers to keep track of the index of suffixes (could use BigInt, but there would be quite some memory
 * overhead).
 */
class MutableSuffixTree extends SuffixTree {
  private val nextCharIdx: AtomicInteger = new AtomicInteger(0)
  private val nodesById: mutable.HashMap[NodeId, Node] = mutable.HashMap()
  private val rootIdsByChar: mutable.HashMap[Char, NodeId] = mutable.HashMap()
  private var nodeId: BigInt = BigInt(0)
  private var activeNodeIds: Set[NodeId] = Set()

  private val generateNodeId: () => NodeId = () => nodeId.synchronized {
    val prev = nodeId
    nodeId += BigInt(1)
    NodeId(prev)
  }

  def addChar(char: Char): MutableSuffixTree = {
    val idx = nextCharIdx.getAndIncrement()
    val newActiveNodes = Set.newBuilder[NodeId]

    activeNodeIds.foreach { activeNodeId =>
      val activeNode = nodesById(activeNodeId)

      if (activeNode.children.contains(char)) {
        newActiveNodes += activeNode.children(char)
        nodesById(activeNode.children(char)).indices += idx
      } else {
        val copy = storeNode(char, idx)
        activeNode.children.put(char, copy.id)
        newActiveNodes += copy.id
      }
    }

    rootIdsByChar.get(char) match {
      case None =>
        val node = storeNode(char, idx)
        rootIdsByChar.put(char, node.id)
        newActiveNodes += node.id
      case Some(rootId) =>
        nodesById(rootId).indices += idx
        newActiveNodes += rootId
    }

    activeNodeIds = newActiveNodes.result()
    this
  }

  override def find(stringView: SeqView[Char, String]): List[Int] = {
    findLongestPrefixOf(stringView) match {
      case (len, node) if len == stringView.length && node.children.isEmpty =>
        node.indices.map(_ + 1 - len).toList
      case _ =>
        List()
    }
  }

  override def findSubstring(stringView: SeqView[Char, String]): List[Int] = {
    findLongestSubstringPrefixOf(stringView) match {
      case Some((len, indices)) if len == stringView.length =>
        indices.map(_ + 1 - len)
      case _ =>
        List()
    }
  }

  override def findLongestSubstringPrefixOf(stringView: SeqView[Char, String]): Option[(Int, List[Int])] = {
    findLongestPrefixOf(stringView) match {
      case (len, node) if len > 0 => Some(len, node.indices.toList)
      case _ => None
    }
  }

  /**
   * Finds the longest prefix of the provided string view that matches (part of) a suffix contained in this tree.
   *
   * @param stringView A character view on a string
   * @return The length of the match and the last matched node
   */
  private def findLongestPrefixOf(stringView: SeqView[Char, String]): (Int, Node) = {
    var len = 0
    var curNode = Node(NodeId(-1), '$', children = rootIdsByChar)

    // Not using `iterator` or `length` since they are linear operations for a SeqView - we want this method to be as
    // fast as possible. Hence the need to use `breakable`
    breakable {
      for (curChar <- stringView) {
        if (curNode.children.contains(curChar)) {
          len += 1
          curNode = nodesById(curNode.children(curChar))
        } else {
          break
        }
      }
    }

    (len, curNode)
  }

  private def storeNode(char: Char, idx: Int): Node = {
    val node = Node(generateNodeId(), char, mutable.LinkedHashSet(idx))
    nodesById.put(node.id, node)
    node
  }
}
