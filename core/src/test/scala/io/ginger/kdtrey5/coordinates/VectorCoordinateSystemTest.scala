package io.ginger.kdtrey5.coordinates

import org.scalatest._
import org.scalatest.Matchers._
import io.ginger.kdtrey5.coordinates.VectorCoordinateSystem.VectorPoint
import org.scalactic.TolerantNumerics

class VectorCoordinateSystemTest extends FunSuite {
  import VectorCoordinateSystem._

  def point(t: (Long, Long)) = VectorPoint(Array(t._1, t._2))
  def within(target: (Long, Long), p1: (Long, Long), p2: (Long, Long), distance: Float) = {
    VectorCoordinateSystem.within(point(target), point(p1), point(p2), VectorDistance(distance))
  }

  test("vector distance") {
    (point(0, 1) |-| point(0, 1)).value shouldBe 0.0f
    (point(0, 1) |-| point(1, 1)).value shouldBe 1.0f
    (point(0, 1) |-| point(1, 2)).value shouldBe (1.41f +- 0.01f)
  }

  test("vector within") {
    within((0, 1), (0, 1), (0, 1), 0) shouldBe true
    within((1, 1), (0, 1), (1, 1), 0) shouldBe true

    within((0, 1), (0, 1), (0, 1), 1) shouldBe true
    within((1, 1), (0, 1), (0, 1), 1) shouldBe true

    within((1, 1), (2, 1), (3, 1), 1) shouldBe true
    within((2, 1), (0, 1), (2, 3), 1) shouldBe true
    within((3, 1), (0, 1), (2, 3), 1) shouldBe true // possible (2, 1)
    within((4, 1), (0, 1), (2, 3), 2) shouldBe true // possible (2, 1)

    within((4, 1), (0, 1), (2, 3), 1) shouldBe false
  }
}
