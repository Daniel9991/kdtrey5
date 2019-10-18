package io.ginger.kdtrey5.coordinates

import java.util.BitSet

/** A coordinate system defines
 *    1) points within a multi-dimensional space,
 *    2) a means to compute distance between those points, and
 *    3) a means to compute common "prefix" between different points,
 *       (based on a definition of most-significant dimensions)
 *       which is expressed as a common plane between those points
 *       (though possibly void, or having zero dimensions)
 */
trait CoordinateSystem {
  // marker traits

  /** A point in multi-dimensional space; defines values for each of the dimensions. */
  trait Point {
    def |-|(other: POINT): DISTANCE
  }

  /** Distance between two points or a point and a plane */
  trait Distance extends Ordered[DISTANCE]

  // corresponding virtual types needing to be set by sub-traits
  type POINT <: Point
  type DISTANCE <: Distance

  def within(target: POINT, p1: POINT, p2: POINT, distance: DISTANCE): Boolean
}
