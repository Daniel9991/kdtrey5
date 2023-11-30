package io.ginger.kdtrey5.coordinates

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import org.scalactic.TolerantNumerics

class VectorCoordinateSystemTest extends AnyFunSuite {
  import VectorCoordinateSystem._
  import VectorCoordinateSystem.ordering.compare

  def point(t: (Long, Long)) = Point(Array(t._1, t._2))

  /*
  def within(target: (Long, Long), p1: (Long, Long), p2: (Long, Long), distance: Float) = {
    (compare(point(target), point(p1)) < distance) &&
      (compare(point(target), point(p2)) < distance)
  }
  */

  test("vector distance") {
    distance(point(0, 1), point(0, 1)) shouldBe 0.0f
    distance(point(0, 1), point(1, 1)) shouldBe 1.0f
    distance(point(0, 1), point(1, 2)) shouldBe (1.41d +- 0.02d)
  }

  test("minDistance 1") {
    minDistance(target = point(0, 1), point(0, 1), point(0, 1)) shouldBe 0
  }

  test("minDistance 2") {
    minDistance(target = point(1, 2), point(1, 5), point(2, 3)) shouldBe 0
  }

  test("minDistance 3") {
    minDistance(target = point(1, 2), point(1, 1), point(1, 5)) shouldBe 0
  }
}
