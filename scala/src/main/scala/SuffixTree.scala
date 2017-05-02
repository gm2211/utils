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
    var len = 0
    var curNode = Node('$', children = rootIdsByChar)

    scala.util.control.Breaks.breakable {
      for (curChar <- string) {
        if (curNode.children.contains(curChar)) {
          len += 1
          curNode = nodesById(curNode.children(curChar))
        } else {
          scala.util.control.Breaks.break
        }
      }
    }

    if (len > 0) Some(len, curNode.indices.toList) else None
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
