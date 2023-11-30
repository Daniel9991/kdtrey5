package io.ginger.kdtrey5.coordinates

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class BinaryCoordinateSystemTest extends AnyFunSuite {
  import BitsetCoordinateSystem._

  test("distance") {
    val p1 = pointFromBinaryString("001001")
    println(s"point is $p1")
    distance(p1,p1) shouldBe 0L

    val p2 = pointFromBinaryString("001011")
    distance(p1, p2) shouldBe 1L

    val p3 = pointFromBinaryString("000011")
    distance(p1, p3) shouldBe 2L

    val p4 = pointFromBinaryString("1001001")
    distance(p1, p4) shouldBe 1L
  }

  test("minDistance 1: target < rangeMin (prefixLen=0)") {
    val target   = pointFromBinaryString("000101")
    val rangeMin = pointFromBinaryString("011001")
    val rangeMax = pointFromBinaryString("100011")
    //  closest  =                        100001
    minDistance(target, rangeMin, rangeMax) shouldBe 1.0d
  }

  test("minDistance 2: target < rangeMin") {
    val target   = pointFromBinaryString("000101")
    val rangeMin = pointFromBinaryString("101001")
    val rangeMax = pointFromBinaryString("110011")
    //  closest  =                        101101
    minDistance(target, rangeMin, rangeMax) shouldBe 2.0d
  }

  test("minDistance 3") {
    val target   = pointFromBinaryString("011001")
    val rangeMin = pointFromBinaryString("001001")
    val rangeMax = pointFromBinaryString("100011")
    //  closest  =                        011001 (distance = 0.0)
    minDistance(target, rangeMin, rangeMax) shouldBe 0.0d
  }

  test("minDistance 4") {
    val target   = pointFromBinaryString("001001")
    val rangeMin = pointFromBinaryString("011001")
    val rangeMax = pointFromBinaryString("100011")
    minDistance(target, rangeMin, rangeMax) shouldBe 1.0d
  }
}
