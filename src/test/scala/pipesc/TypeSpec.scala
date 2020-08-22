package pipesc

import scala.util.parsing.input.OffsetPosition

import org.scalatest._

class TypeSpec extends FlatSpec with Matchers {

  "merge" should "merge identical intervals into one" in {
    Type.merge(Seq(MinMax(0, 1), MinMax(0, 1))) shouldEqual Seq(MinMax(0, 1))
  }

  it should "merge overlapping intervals into one" in {
    Type.merge(Seq(MinMax(0, 1), MinMax(1, 2))) shouldEqual Seq(MinMax(0, 2))
  }

  it should "merge adjacent intervals into one" in {
    Type.merge(Seq(MinMax(0, 1), MinMax(2, 3))) shouldEqual Seq(MinMax(0, 3))
  }

  it should "merge three adjacent intervals into one" in {
    Type.merge(Seq(MinMax(0, 1), MinMax(2, 3), MinMax(4, 4))) shouldEqual Seq(MinMax(0, 4))
  }

  it should "not merge non-adjacent simple intervals into one" in {
    Type.merge(Seq(MinMax(-1, -1), MinMax(1, 1))) shouldEqual Seq(MinMax(-1, -1), MinMax(1, 1))
  }

  it should "merge interval fully encapsulated by other interval into that interval" in {
    Type.merge(Seq(MinMax(0, 10), MinMax(2, 3))) shouldEqual Seq(MinMax(0, 10))
  }

  it should "merge negative intervals" in {
    Type.merge(Seq(MinMax(-100, 10), MinMax(9, 1000))) shouldEqual Seq(MinMax(-100, 1000))
  }

  it should "merge non-sorted intervals" in {
    Type.merge(Seq(MinMax(10, 20), MinMax(0, 11))) shouldEqual Seq(MinMax(0, 20))
  }

  it should "do nothing to an empty sequence" in {
    Type.merge(Seq()) shouldEqual Seq()
  }

  it should "do nothing to a single interval" in {
    Type.merge(Seq(MinMax(0,1))) shouldEqual Seq(MinMax(0,1))
  }

}
