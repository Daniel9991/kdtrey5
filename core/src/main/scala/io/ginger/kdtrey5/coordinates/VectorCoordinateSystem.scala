package io.ginger.kdtrey5.coordinates

import java.util.BitSet

object VectorCoordinateSystem extends CoordinateSystem {
  type DISTANCE = VectorDistance
  type POINT = VectorPoint
  type PLANE = VectorPlane

  case class VectorDistance(value: Float) extends super.Distance {
    override def compare(that: DISTANCE): Int = {
      if (this.value > that.value) 1
      else if (this.value == that.value) 0
      else -1
    }
  }

  implicit val vectorOrdering = new Ordering[POINT] {
    override def compare(x: VectorPoint, y: VectorPoint): Int = {
      val a1 = x.values
      val a2 = y.values
      if (a1.length != a2.length) throw new Exception("Cannot compare points of different lengths")
      var pos = a1.length - 1
      while (pos >= 0) {
        val v1 = a1(pos)
        val v2 = a2(pos)
        if ( v1 > v2) return  1
        if ( v1 < v2) return -1
        pos -= 1
      }
      return 0
    }
  }

  case class VectorPoint(val values: Array[Float]) extends super.Point {
    override def toString = {
      s"VectorPoint(${values.toSeq.mkString(",")})"
    }
  }

  case class VectorPlane(val values: Array[Float]) extends super.Plane {
    override def toString = {
      s"VectorPlane(${values.toSeq.mkString(",")})"
    }
  }

  override def distance(p1: POINT, p2: POINT): DISTANCE = {
    if (p1.values.length != p2.values.length) throw new Exception(s"Can't compare: $p1 $p2")
    // calculate dot-product
    var i = 0
    var distance = 0.0f
    while (i < p1.values.length) {
      distance += p1.values(i) * p2.values(i)
      i += 1
    }
    VectorDistance(distance)
  }

  override def distance(p1: PLANE, p2: PLANE): DISTANCE = {
    if (p1.values.length != p2.values.length) throw new Exception(s"Can't compare: $p1 $p2")
    // calculate dot-product
    var i = 0
    var distance = 0.0f
    while (i < p1.values.length) {
      distance += p1.values(i) * p2.values(i)
      i += 1
    }
    VectorDistance(distance)
  }

  private def dotProduct(a1: Array[Float], a2: Array[Float]): Float = {
    if (a1.length != a2.length) throw new Exception(s"Invalid lengths: $a1 $a2")
    var i = 0
    var distance = 0.0f
    while (i < a1.length) {
      distance += a1(i) * a2(i)
      i += 1
    }
    return distance
  }

  override def distance(p1: POINT, p2: PLANE): DISTANCE = {
    VectorDistance(dotProduct(p1.values, p2.values))
  }

  override def commonPrefixFromPoints(p1: POINT, p2: POINT): PLANE = {
    ???
  }
}