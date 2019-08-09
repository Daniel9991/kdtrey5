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
  trait Point

  /** A plane in a multi-dimensional space; some (or all) dimensions may be undefined. */
  trait Plane

  /** Distance between two points or a point and a plane */
  trait Distance extends Ordered[DISTANCE]

  // corresponding virtual types needing to be set by sub-traits
  type POINT <: Point
  type PLANE <: Plane
  type DISTANCE <: Distance

  /** Calculate the distance between two points */
  def distance(p1: POINT, p2: POINT): DISTANCE

  /** Calculate the (shortest) minimum distance between two planes */
  def distance(p1: PLANE, p2: PLANE): DISTANCE

  /** Calculate the (shortest) distance between a point and a plane */
  def distance(p1: POINT, p2: PLANE): DISTANCE
  def distance(p1: PLANE, p2: POINT): DISTANCE = distance(p2, p1)

  /** Calculate the common prefix (dimensions) between two points */
  def commonPrefixFromPoints(p1: POINT, p2: POINT): PLANE

}
