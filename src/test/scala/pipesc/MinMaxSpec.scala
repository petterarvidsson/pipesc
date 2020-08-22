package pipesc

import org.scalatest._

class MinMaxSpec extends FlatSpec with Matchers {

  "MinMax.overlaps" should "be true for [0, 1], [0, 1]" in {
    MinMax.overlaps(MinMax(0, 1), MinMax(0, 1)) shouldEqual true
  }

  it should "be true for [0, 1], [1, 2]" in {
    MinMax.overlaps(MinMax(0, 1), MinMax(1, 2)) shouldEqual true
  }

  it should "be true for [1, 1], [2, 2]" in {
    MinMax.overlaps(MinMax(1, 1), MinMax(2, 2)) shouldEqual true
  }

  it should "be true for [1, 2], [0, 1]" in {
    MinMax.overlaps(MinMax(1, 2), MinMax(0, 1)) shouldEqual true
  }

  it should "be true for [1, 2], [-1, 0]" in {
    MinMax.overlaps(MinMax(1, 2), MinMax(-1, 0)) shouldEqual true
  }

  it should "be false for [1, 2], [-1, -1]" in {
    MinMax.overlaps(MinMax(1, 2), MinMax(-1, -1)) shouldEqual false
  }

  it should "be true for [0, 1], [2, 3]" in {
    MinMax.overlaps(MinMax(0, 1), MinMax(2, 3)) shouldEqual true
  }

  it should "be false for [0, 1], [3, 4]" in {
    MinMax.overlaps(MinMax(0, 1), MinMax(3, 4)) shouldEqual false
  }

  it should "be false for [1, 1], [3, 3]" in {
    MinMax.overlaps(MinMax(1, 1), MinMax(3, 3)) shouldEqual false
  }

}
