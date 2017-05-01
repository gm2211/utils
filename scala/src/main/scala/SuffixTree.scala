import java.util.UUID

import scala.collection.mutable

case class Node(
  char: Char,
  indices: mutable.LinkedHashSet[Int] = mutable.LinkedHashSet(),
  children: mutable.HashMap[Char, UUID] = mutable.HashMap(),
  id: UUID = UUID.randomUUID()
)

class SuffixTree {
  val nodesById: mutable.HashMap[UUID, Node] = mutable.HashMap()
  val rootIdsByChar: mutable.HashMap[Char, UUID] = mutable.HashMap()
  var activeNodeIds: Set[UUID] = Set()

  def addChar(char: Char, idx: Int): SuffixTree = {
    val node = Node(char, mutable.LinkedHashSet(idx))
    val newActiveNodes = Set.newBuilder[UUID]

    nodesById.put(node.id, node)
    newActiveNodes += node.id

    activeNodeIds.foreach { activeNodeId =>
      val activeNode = nodesById(activeNodeId)

      if (activeNode.children.contains(char)) {
        newActiveNodes += activeNode.children(char)
        nodesById(activeNode.children(char)).indices += idx
      } else {
        activeNode.children.put(char, node.id)
        newActiveNodes += node.id
      }
    }

    rootIdsByChar.get(char) match {
      case None => rootIdsByChar.put(char, node.id)
      case Some(rootId) => newActiveNodes += rootId; nodesById(rootId).indices += idx
    }

    activeNodeIds = newActiveNodes.result()
    this
  }

  def longestPrefixLen(string: String): Option[(Int, List[Int])] = {
    val initialValue: (Int, Node) = (0, Node('$', children = rootIdsByChar))

    val (maxLen, lastNode): (Int, Node) = string.scanLeft(initialValue) {
      // Try to continue matching string character from the current node
      case ((len, node), char) if node.children.contains(char) => (len + 1, nodesById(node.children(char)))
      // Stop if there are still characters in the string but no child was matched in the previous iteration
      case _ => (-1, Node('$'))
    }.takeWhile{ case (len, _) => len >= 0 }
     .last

    if (maxLen > 0) {
      val startIndices = lastNode.indices.map(_ + 1 - maxLen)
      Some((maxLen, startIndices.toList))
    } else {
      None
    }
  }

  def find(string: String): List[Int] = {
    longestPrefixLen(string) match {
      case Some((len, indices)) if len == string.length => indices
      case _ => List()
    }
  }
}

object SuffixTree {

  def build(string: String): SuffixTree = {
    string.zipWithIndex.foldLeft(new SuffixTree()) {
      case (tree, (char, idx)) => tree.addChar(char, idx)
    }
  }
}
