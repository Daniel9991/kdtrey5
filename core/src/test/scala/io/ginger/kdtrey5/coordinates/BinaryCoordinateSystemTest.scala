package io.ginger.kdtrey5.coordinates

import org.scalatest._
import org.scalatest.Matchers._

class BinaryCoordinateSystemTest extends FunSuite {
  import BitsetCoordinateSystem._

  test("distance") {
    val p1 = pointFromBinaryString("001001")
    println(s"point is $p1")
    distance(p1, p1).value shouldBe 0L

    val p2 = pointFromBinaryString("001011")
    distance(p1, p2).value shouldBe 1L

    val p3 = pointFromBinaryString("000011")
    distance(p1, p3).value shouldBe 2L

    val p4 = pointFromBinaryString("1001001")
    distance(p1, p4).value shouldBe 1L
  }

  test("plane-plane distance") {
    locally {
      val p = planeFromBinaryString("001xx1")
      distance(p, p).value shouldBe 0L
    }
    locally {
      val p1 = planeFromBinaryString("001xx1")
      val p2 = planeFromBinaryString("10xxx1")
      distance(p1, p2).value shouldBe 1L
    }
  }

  test("point-plane distance") {
    locally {
      val plane = planeFromBinaryString("001xx1")
      val point = pointFromBinaryString("001011")
      distance(point, plane).value shouldBe 0L
    }
    locally {
      val plane = planeFromBinaryString("00xx01")
      val point = pointFromBinaryString("101011")
      distance(point, plane).value shouldBe 2L
    }
    locally {
      val plane = planeFromBinaryString("0xxxx")
      val point = pointFromBinaryString("11111")
      distance(point, plane).value shouldBe 1L
    }
  }

  test("commonPrefix") {
    locally {
      val p1 = pointFromBinaryString("01001")
      val p2 = pointFromBinaryString("00011")
      val prefix = commonPrefixFromPoints(p1, p2)
      prefix shouldBe planeFromBinaryString("0xxxx")
      val point = pointFromBinaryString("11111")
      distance(point, prefix).value shouldBe 1L
    }
  }
}