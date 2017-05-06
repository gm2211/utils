package com.gm2211.utils.scala.datastructures.string.suffixtree

import scala.collection.mutable

case class Node(
  id: NodeId,
  char: Char,
  indices: mutable.LinkedHashSet[Int] = mutable.LinkedHashSet(),
  children: mutable.HashMap[Char, NodeId] = mutable.HashMap()
)

case class NodeId(value: BigInt)
