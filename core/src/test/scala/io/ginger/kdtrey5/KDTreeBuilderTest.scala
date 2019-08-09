package io.ginger.kdtrey5

import io.ginger.kdtrey5.data._
import io.ginger.kdtrey5.mapreduce._

import org.scalatest._
import org.scalatest.Matchers._

import scala.reflect.ClassTag


class KDTreeBuilderTest extends FunSuite {

  val builder = new KDTreeBuilder {
    override def newDataset[T](): Dataset[T] = InMemoryDataset(Iterable.empty)
  }

  test("simple build") {
    val input = Seq(
      "3" -> "three", 
      "2" -> "two",
      "5" -> "five", 
      "4" -> "four",
      "1" -> "one", 
      "7" -> "seven", 
      "6" -> "six", 
      "8" -> "eight", 
      "9" -> "nine")
    val output = builder.build(InMemoryDataset(input), fanout = 2)
    val expected = Set(
      leaf("Leaf#0-0",
        "1" -> "one",
        "2" -> "two",
      ),
      leaf("Leaf#0-1",
        "3" -> "three",
        "4" -> "four",
      ),
      leaf("Leaf#0-2",
        "5" -> "five",
        "6" -> "six",
      ),
      leaf("Leaf#0-3",
        "7" -> "seven",
        "8" -> "eight",
      ),
      leaf("Leaf#0-4",
        "9" -> "nine",
      ),
      // level 1
      branch("Branch#1-0-0",
        "1" -> "Leaf#0-0",
        "3" -> "Leaf#0-1"
      ),
      branch("Branch#1-0-1",
        "5" -> "Leaf#0-2",
        "7" -> "Leaf#0-3"
      ),
      branch("Branch#1-0-2",
        "9" -> "Leaf#0-4"
      ),
      // level 2
      branch("Branch#2-0-0",
        "1" -> "Branch#1-0-0",
        "5" -> "Branch#1-0-1"
      ),
      branch("Branch#2-0-1",
        "9" -> "Branch#1-0-2",
      ),
      // level 3
      branch("Branch#3-0-0",
        "1" -> "Branch#2-0-0",
        "9" -> "Branch#2-0-1",
      )
    )

    output.rootId shouldBe "Branch#3-0-0"
    output.nodes.toSeq.toSet shouldBe expected
  }

  def leaf[K, V](id: String, values: (K, V)*)(implicit ktag: ClassTag[K], vtag: ClassTag[V]) = {
    KDLeaf(
      id, 
      keys   = values.map(_._1).toArray,
      values = values.map(_._2).toArray,
      size = values.size
    )
  }
  def branch[K](id: String, values: (K, NodeId)*)(implicit ktag: ClassTag[K]) = {
    KDBranch(
      id, 
      keys   = values.map(_._1).toArray,
      nodes  = values.map(_._2).toArray,
      size = values.size
    )
  }
}