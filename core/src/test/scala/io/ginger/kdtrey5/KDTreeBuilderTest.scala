package io.ginger.kdtrey5

import io.ginger.kdtrey5.data._
import io.ginger.kdtrey5.mapreduce._

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

import scala.reflect.ClassTag
import io.ginger.kdtrey5.coordinates.VectorCoordinateSystem
import io.ginger.kdtrey5.coordinates.VectorCoordinateSystem.Point

class KDTreeBuilderTest extends AnyFunSuite {
  def point(value: Int) = Point(Array(value))

  val builder = new KDTreeBuilder {
    override def newDataset[T](): Dataset[T] = InMemoryDataset(Iterable.empty)
  }

  test("simple build") {
    val input = Seq(
      3 -> "three",
      2 -> "two",
      5 -> "five",
      4 -> "four",
      1 -> "one",
      7 -> "seven",
      6 -> "six",
      8 -> "eight",
      9 -> "nine"
    ).map { case (key, value) => point(key) -> value}

    val output = builder.build(InMemoryDataset(input), fanout = 2, VectorCoordinateSystem)
    val expected: Set[KDNode[Point, String]] = Set(
      leaf("Leaf#0-0", point(1) -> "one",   point(2) -> "two"),
      leaf("Leaf#0-1", point(3) -> "three", point(4) -> "four"),
      leaf("Leaf#0-2", point(5) -> "five",  point(6) -> "six"),
      leaf("Leaf#0-3", point(7) -> "seven", point(8) -> "eight"),
      leaf("Leaf#0-4", point(9) -> "nine"),
      // level 1
      branch("Branch#1-0-0", ("Leaf#0-0", 1), ("Leaf#0-1", 3))(lastKey = 4),
      branch("Branch#1-0-1", ("Leaf#0-2", 5), ("Leaf#0-3", 7))(lastKey = 8),
      branch("Branch#1-0-2", ("Leaf#0-4", 9))(lastKey = 9),
      // level 2
      branch("Branch#2-0-0", ("Branch#1-0-0", 1), ("Branch#1-0-1", 5))(lastKey = 8),
      branch("Branch#2-0-1", ("Branch#1-0-2", 9))(lastKey = 9),
      // level 3
      branch("Branch#3-0-0", ("Branch#2-0-0", 1), ("Branch#2-0-1", 9))(lastKey = 9)
    )

    //output.rootId shouldBe "Branch#3-0-0"
    val actual = output.nodes.toSeq.toSet
    if (actual != expected) {
      println("actual: " + actual)
      println("actual -- expected: " + (actual -- expected))
      println("expected -- actual: " + (expected -- actual))
    }
    output.nodes.toSeq.toSet shouldBe expected
  }

  def leaf[K, V](id: String, values: (K, V)*)(implicit ktag: ClassTag[K], vtag: ClassTag[V]) = {
    KDLeaf(
      id,
      keys = values.map(_._1).toArray,
      values = values.map(_._2).toArray,
    )
  }

  def branch(id: String, values: (NodeId, Int)*)(lastKey: Int) = {
    KDBranch[Point, String](
      id,
      nodes = values.map(_._1).toArray,
      keys = (values.map(t => point(t._2)) :+ point(lastKey)).toArray,
    )
  }
}
