package com.gm2211.collections

import scala.collection.mutable

object MapExtensions {

  implicit class TwoMapsRecursiveMerger(maps: (Map[String, Any], Map[String, Any])) {

    def recursiveMergeMaps(): Map[String, Any] = (recursiveMergeMapsHelper _).tupled(maps)

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

  implicit class TwoMapsIterativeRecursiveMerger(maps: (Map[String, Any], Map[String, Any])) {

    def recursiveIterativeMergeMaps(): Map[String, Any] = (recursiveMergeMapsHelper _).tupled(maps)

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
}
