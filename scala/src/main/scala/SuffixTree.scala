import java.util.concurrent.atomic.AtomicLong

import scala.collection.mutable

case class Node(
  id: Long,
  char: Char,
  indices: mutable.LinkedHashSet[Int] = mutable.LinkedHashSet(),
  children: mutable.HashMap[Char, Long] = mutable.HashMap()
)

class SuffixTree {
  val curNodeId: AtomicLong = new AtomicLong(-1)
  val nodesById: mutable.HashMap[Long, Node] = mutable.HashMap()
  val rootIdsByChar: mutable.HashMap[Char, Long] = mutable.HashMap()
  var activeNodeIds: Set[Long] = Set()

  def addChar(char: Char, idx: Int): SuffixTree = {
    val newActiveNodes = Set.newBuilder[Long]

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

  private def storeNode(char: Char, idx: Int): Node = {
    val node = Node(curNodeId.incrementAndGet(), char, mutable.LinkedHashSet(idx))
    nodesById.put(node.id, node)
    node
  }

  def longestPrefixLen(string: String): Option[(Int, List[Int])] = {
    var len = 0
    var curNode = Node(-1, '$', children = rootIdsByChar)

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
      case Some((len, indices)) if len == string.length => indices.map(_ + 1 - len)
      case _ => List()
    }
  }

  case class ResolvedNode(
    id: Long,
    char: Char,
    children: Map[Char, ResolvedNode],
    indices: mutable.LinkedHashSet[Int]
  )
  def resolve: (Long) => ResolvedNode = { id =>
    val node: Node = nodesById(id)
    ResolvedNode(node.id, node.char, node.children.mapValues(resolve).toMap, node.indices)
  }

  def resolved(): Map[Char, Product with Serializable with Object] = {
    rootIdsByChar
      .mapValues(resolve)
      .toMap
  }
}

object SuffixTree {
  def build(string: String): SuffixTree = {
    string.zipWithIndex.foldLeft(new SuffixTree()) {
      case (tree, (char, idx)) => tree.addChar(char, idx)
    }
  }
}
