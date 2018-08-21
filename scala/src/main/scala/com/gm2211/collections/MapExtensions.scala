package com.gm2211.collections

import scala.collection.mutable
import scala.reflect.ClassTag

object MapExtensions {

  /**
    * Allows for more readable sintax when checking if a key is part of a collection.
    */
  implicit class CandidateCollectionEntry[T](value: T) {
    def in(set: Set[T]): Boolean = set.contains(value)

    def isKeyOf(map: scala.collection.Map[T, Any]): Boolean = map.contains(value)

    def isNotKeyOf(map: scala.collection.Map[T, Any]): Boolean = !map.contains(value)
  }

  /**
    * Recursive implementation of deep merging of maps.
    */
  implicit class DeepMergerRecursive(maps: (Map[String, Any], Map[String, Any])) {

    def deepMergeRecursive(): Map[String, Any] = (recursiveMergeMapsHelper _).tupled(maps)

    private def recursiveMergeMapsHelper(left: Map[String, Any], right: Map[String, Any]): Map[String, Any] = {
      val mergedMap = mutable.Map[String, Any]()
      val commonKeys = left.keySet intersect right.keySet
      val leftOnlyKeys = left.keySet diff right.keySet
      val rightOnlyKeys = right.keySet diff left.keySet
      val commonNestedMaps = Stack[(String, Map[String, Any], Map[String, Any])]()

      for (leftKey <- leftOnlyKeys) {
        mergedMap += leftKey -> left(leftKey)
      }

      for (rightKey <- rightOnlyKeys) {
        mergedMap += rightKey -> right(rightKey)
      }

      for (commonKey <- commonKeys) {
        val leftValue = left(commonKey)
        val rightValue = right(commonKey)

        assert(
          leftValue.getClass.isAssignableFrom(rightValue.getClass),
          s"Incompatible types for values of '$commonKey'"
        )

        leftValue match {
          case _: Map[_, _] =>
            commonNestedMaps.push(
              (commonKey, leftValue.asInstanceOf[Map[String, Any]], rightValue.asInstanceOf[Map[String, Any]])
            )
          case list: List[_] => mergedMap += commonKey -> (list union rightValue.asInstanceOf[List[Any]])
          case _ => mergedMap += commonKey -> rightValue // right map overrides
        }
      }

      while (commonNestedMaps.nonEmpty) {
        val (nestedMapKey, leftNested, rightNested) = commonNestedMaps.pop()
        mergedMap += nestedMapKey -> recursiveMergeMapsHelper(leftNested, rightNested)
      }

      mergedMap.toMap
    }
  }

  /**
    * Iterative implementation of deep merging maps (uses an accumulator and returns mutable maps).
    *
    * // TODO(gmecocci): if we use a stack and a queue like in deep converter we can probably produce immutable nested
    * maps instead of calling deepConvert on it at the end (which means we're traversing nodes
    * unnecessarily many times).
    */
  implicit class DeepMergerIterative(maps: (Map[String, Any], Map[String, Any])) {

    def deepMergeIterative(): Map[String, Any] = (recursiveMergeMapsHelper _).tupled(maps)
      .deepConvert { case merged: mutable.Map[_, _] => merged.toMap }

    private def recursiveMergeMapsHelper(left: Map[String, Any], right: Map[String, Any]): Map[String, Any] = {
      val rootAccumulator = mutable.Map[String, Any]()
      val mapsBeingMerged = mutable.Queue[(mutable.Map[String, Any], Map[String, Any], Map[String, Any])]()

      mapsBeingMerged.enqueue((rootAccumulator, left, right))

      while (mapsBeingMerged.nonEmpty) {
        val (localAccumulator, left, right) = mapsBeingMerged.dequeue()
        val commonKeys = left.keySet intersect right.keySet
        val leftOnlyKeys = left.keySet diff right.keySet
        val rightOnlyKeys = right.keySet diff left.keySet

        for (leftKey <- leftOnlyKeys) {
          localAccumulator += leftKey -> left(leftKey)
        }

        for (rightKey <- rightOnlyKeys) {
          localAccumulator += rightKey -> right(rightKey)
        }

        for (commonKey <- commonKeys) {
          val leftValue = left(commonKey)
          val rightValue = right(commonKey)

          assert(
            leftValue.getClass.isAssignableFrom(rightValue.getClass),
            s"Incompatible types for values of '$commonKey'"
          )

          leftValue match {
            case _: Map[_, _] =>
              val newAccumulator = mutable.Map[String, Any]()
              localAccumulator += commonKey -> newAccumulator
              mapsBeingMerged.enqueue(
                (newAccumulator, leftValue.asInstanceOf[Map[String, Any]], rightValue.asInstanceOf[Map[String, Any]])
              )
            case list: List[_] => localAccumulator += commonKey -> (list union rightValue.asInstanceOf[List[Any]])
            case _ => localAccumulator += commonKey -> rightValue // right map overrides
          }
        }
      }

      rootAccumulator.toMap
    }
  }

  /**
    * Takes a map and:
    * 1. Converts all nested maps to the desired output map type.
    * 2. Applies the provided partial mapping function to all nested values.
    */
  implicit class DeepConverter[K : ClassTag](map: scala.collection.Map[K, Any]) {
    import scala.reflect._

    sealed trait Node
    sealed trait StackNode

    case class ValueNode(
      value: Any,
      keyInParent: K,
      keyOfParent: Option[K] = None,
      depth: Int = 0
    ) extends Node with StackNode

    case class MapNode(
      map: scala.collection.Map[K, Any],
      keyInParent: Option[K] = None,
      keyOfParent: Option[K] = None,
      depth: Int = 0
    ) extends Node

    case class SimpleMapNode(
      keyInParent: Option[K] = None,
      keyOfParent: Option[K] = None,
      depth: Int = 0
    ) extends StackNode


    def mapConverter[
      IN <: scala.collection.Map[K, Any],
      OUT <: scala.collection.Map[K, Any]
    ](m: IN)(
      implicit inTag: ClassTag[IN], outTag: ClassTag[OUT]
    ): OUT = m match {
      case _ if inTag.runtimeClass.equals(outTag.runtimeClass) =>
        m.asInstanceOf[OUT]
      case m: Map[_, _] if outTag.runtimeClass.equals(classOf[mutable.Map[_, _]]) =>
        mutable.Map(m.toSeq:_*).asInstanceOf[OUT]
      case m: mutable.Map[_, _] if outTag.runtimeClass.equals(classOf[Map[_, _]]) =>
        m.toMap.asInstanceOf[OUT]
    }

    def deepConvert[OUTPUT_MAP <: scala.collection.Map[K, Any]](converter: PartialFunction[Any, Any])
      (implicit outputTag: ClassTag[OUTPUT_MAP]): OUTPUT_MAP = {

      def applyConverter(value: Any): Any = converter.applyOrElse(value, identity[Any])

      val stack = Stack[StackNode]()
      val queue = mutable.Queue[Node]()

      queue.enqueue(MapNode(map))

      while (queue.nonEmpty) {
        val node = queue.dequeue()

        node match {
          case MapNode(mapInNode, parentKeyInParent, parentKeyOfParent, parentDepth) =>
            stack.push(SimpleMapNode(parentKeyInParent, parentKeyOfParent, parentDepth))

            mapInNode.toSeq.foreach {
              case (keyOfValue: K, elem: scala.collection.Map[_, _]) => queue.enqueue(
                MapNode(
                  elem.map { case (key, value) => key.asInstanceOf[K] -> value }.toMap,
                  Some(keyOfValue),
                  parentKeyInParent,
                  parentDepth + 1
                )
              )
              case (keyOfValue: K, value: Any) => queue.enqueue(
                ValueNode(
                  value,
                  keyOfValue,
                  parentKeyInParent,
                  parentDepth + 1
                )
              )
            }

          case v@ValueNode(_, _, _, _) => stack.push(v)
        }
      }

      val accumulators: mutable.Map[(K, Int), mutable.Map[K, Any]] = mutable.Map()
      val rootMap: mutable.Map[K, Any] = mutable.Map()

      while (stack.nonEmpty) {
        val node = stack.pop()

        node match {
          case ValueNode(value, keyInParent, Some(keyOfParent), depth) =>
            if ((keyOfParent, depth) isNotKeyOf accumulators) {
              accumulators += (keyOfParent, depth) -> mutable.Map[K, Any]()
            }
            accumulators((keyOfParent, depth)) += keyInParent -> applyConverter(value)

          case ValueNode(value, keyInParent, None, _) =>
            rootMap += keyInParent -> applyConverter(value)

          case SimpleMapNode(Some(keyInParent), Some(keyOfParent), depth) =>
            assert((keyInParent, depth + 1) isKeyOf accumulators)
            if ((keyOfParent, depth) isNotKeyOf accumulators) {
              accumulators += (keyOfParent, depth) -> mutable.Map[K, Any]()
            }

            accumulators((keyOfParent, depth)) += keyInParent -> applyConverter(
              mapConverter(accumulators((keyInParent, depth + 1)))(classTag[mutable.Map[K, Any]], outputTag)
            )

          case SimpleMapNode(Some(keyInParent), None, depth) =>
            assert((keyInParent, depth + 1) isKeyOf accumulators)

            val value = applyConverter(
              mapConverter(accumulators((keyInParent, depth + 1)))(classTag[mutable.Map[K, Any]], outputTag)
            )

            rootMap += keyInParent -> value
          case _ => // do nothing
        }
      }

      mapConverter(rootMap.toMap)(classTag[Map[K, Any]], outputTag)
    }
  }
}
