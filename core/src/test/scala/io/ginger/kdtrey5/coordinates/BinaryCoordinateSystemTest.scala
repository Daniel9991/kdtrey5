package io.ginger.kdtrey5.coordinates

import org.scalatest._
import org.scalatest.Matchers._

class BinaryCoordinateSystemTest extends FunSuite {
  import BitsetCoordinateSystem._

  test("distance") {
    val p1 = pointFromBinaryString("001001")
    println(s"point is $p1")
    (p1 |-| p1).value shouldBe 0L

    val p2 = pointFromBinaryString("001011")
    (p1 |-| p2).value shouldBe 1L

    val p3 = pointFromBinaryString("000011")
    (p1 |-| p3).value shouldBe 2L

    val p4 = pointFromBinaryString("1001001")
    (p1 |-| p4).value shouldBe 1L
  }

  def within(target: String, p1: String, p2: String, distance: Int) = {
    BitsetCoordinateSystem.within(
      pointFromBinaryString(target),
      pointFromBinaryString(p1),
      pointFromBinaryString(p2),
      BitsetDistance(distance))
  }

  test("within") {
    within("101", "101", "110", 0) shouldBe true
    within("101", "101", "110", 1) shouldBe true
    within("101", "100", "110", 1) shouldBe true
    within("000", "100", "110", 1) shouldBe true
    within("000", "000", "110", 1) shouldBe true
    within("100", "000", "110", 1) shouldBe true
    within("111", "000", "110", 1) shouldBe true
    within("111", "000", "100", 1) shouldBe true
    within("111", "000", "001", 2) shouldBe true
    within("111", "000", "000", 3) shouldBe true

    within("111", "000", "001", 1) shouldBe false
    within("111", "000", "000", 1) shouldBe false
    within("111", "000", "000", 2) shouldBe false

    within(
      "010010101100110100100101011100001001111001010000010000000110100100011100000110111000011111000110011100100110001111011001001110",
      "111111100001010000001001100111100000000000110011101011101100001011000111101100110011111001101010000011001101111110000101001100",
      "111111100011100010001110011000000100011011100000101101111001101111110000011110111010001001111011100111101111010001000010110011",
    // 1 23 4  67
      6
    ) shouldBe true

    within(
      "0100101011001",
      "1111111000010",
      "1111111000111",
    // 1 23 4  56
      6
    ) shouldBe true

    within(
      "0100101011001",
      "1111111000010",
      "1111111000111",
    // 1 23 4  567!
      5
    ) shouldBe false
  }

  /*
  test("distanceOverCommonPrefix") {
    def distance(target: String, prefix1: String, prefix2: String): Long = {
      distanceOverCommonPrefix(
        pointFromBinaryString(target),
        pointFromBinaryString(prefix1),
        pointFromBinaryString(prefix2)
      ).value
    }
    distance("01001", "010001", "00011") shouldBe 0
  }
  */
}
