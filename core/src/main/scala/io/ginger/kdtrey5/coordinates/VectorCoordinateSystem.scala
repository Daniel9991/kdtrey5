package io.ginger.kdtrey5.coordinates

import java.util.BitSet
import io.ginger.kdtrey5.Utils.arrayEquals

/**
 * An arbitrary-length vector coordinate system.
 *
 * e.g. points can be Array(1,2), Array(1,2,3), ...
 *
 * Distance is define as dot-product between two points.
 */
object VectorCoordinateSystem extends CoordinateSystem {
  override type POINT = Point

  type Coordinate = Double

  /** A point in multi-dimensional space. */
  final case class Point(val coordinates: Array[Coordinate]) {
    /** Implementation note:
     *  Ideally this would be a 'value class', e.g., extends AnyVal
     *  but for testing we want to test for equality and value classes
     *  can't override equals(), and arrays don't natively compare by value
     */
    override def toString = {
      s"Point(${coordinates.toSeq.mkString(",")})"
    }
    override def equals(other: Any) = other match {
      case other: Point =>
        arrayEquals(this.coordinates, other.coordinates, coordinates.size)
      case _ => false
    }

  }

  object Point {
    implicit val ordering: Ordering[Point] = VectorCoordinateSystem.ordering
  }

  override val ordering: Ordering[Point] = new Ordering[Point] {
    override def compare(x: Point, y: Point): Int = {
      val a1 = x.coordinates
      val a2 = y.coordinates
      if (a1.length != a2.length) throw new Exception("Cannot compare points of different lengths")
      var pos = 0
      while (pos < a1.length) {
        val v1 = a1(pos)
        val v2 = a2(pos)
        if (v1 > v2) return 1
        if (v1 < v2) return -1
        pos += 1
      }
      return 0
    }
  }

  override def distance(p1: Point, p2: Point): Distance = {
    if (p1.coordinates.length != p2.coordinates.length)
      throw new Exception(s"Can't compare: $p1 $p2")
    var i = 0
    var sumOfSquares = 0.0d
    while (i < p1.coordinates.length) {
      val diff = p1.coordinates(i) - p2.coordinates(i)
      sumOfSquares += (diff * diff)
      i += 1
    }
    Math.sqrt(sumOfSquares)
  }

  override def minDistance(target: POINT, rangeMin: POINT, rangeMax: POINT): Distance = {
    var i = 0
    var continue = true
    var sumOfSquares = 0.0d
    while (i < target.coordinates.length && continue) {
      val t = target.coordinates(i)
      val min = rangeMin.coordinates(i)
      val max = rangeMax.coordinates(i)

      if (min == max) {
        val diff = t - min
        sumOfSquares += (diff * diff)
      } else {
        val closest =
          if (t <= min) min
          else if (t >= max ) max
          else t
        val diff = t - closest
        sumOfSquares += (diff * diff)
        continue = false
      }
      i += 1
    }

    Math.sqrt(sumOfSquares)
  }


  def dotProduct(p1: POINT, p2: POINT): Distance = {
    if (p1.coordinates.length != p2.coordinates.length) throw new Exception(s"Can't compare: $p1 $p2")
    var i = 0
    var distance = 0.0d
    while (i < p1.coordinates.length) {
      distance += p1.coordinates(i) * p2.coordinates(i)
      i += 1
    }
    distance
  }

}
