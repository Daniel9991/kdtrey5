package io.ginger.kdtrey5.coordinates

/** A coordinate system defines how to compute distance between points in a multi-dimensional space.
 *  This could be Euclidean distance, Manhattan distance, etc.
 */
trait CoordinateSystem {
  type Distance = CoordinateSystem.Distance

  /** Abstract point in coordinate system */
  type POINT

  /** A strict ordering of POINTs; needed for K-d tree construction */
  val ordering: Ordering[POINT]

  /** Returns the distance between this point and another point */
  def distance(p1: POINT, p2: POINT): Distance

  /** Returns the minimum distance between `target` point and area bounded dimension-wise by rangeMin and rangeMax */
  def minDistance(target: POINT, rangeMin: POINT, rangeMax: POINT): Distance

}

object CoordinateSystem {
  type Distance = Double
}