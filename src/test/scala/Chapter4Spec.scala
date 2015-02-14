import org.scalatest._

import Chapter4._

class Chapter4Spec extends FlatSpec with Matchers {
  "variance of empty list" should "be None" in {
    variance(Nil) should be (None)
  }

  "variance of [2, 2, 2]" should "be zero" in {
    variance(List(2, 2, 2)) should be (Some(0))
  }

  "sequence of empty list" should "be Some(Nil)" in {
    sequence(Nil) should be (Some(Nil))
  }

  "sequence of List(Some(1))" should "be Some(List(1))" in {
    sequence(List(Some(1))) should be (Some(List(1)))
  }

  "sequence of List(Some(1), None, Some(3))" should "be None" in {
    sequence(List(Some(1), None, Some(3))) should be (None)
  }

  "sequence of List(Some(1), Some(2), Some(3))" should "be None" in {
    sequence(List(Some(1), Some(2), Some(3))) should be (Some(List(1, 2, 3)))
  }

  "sequence1 of empty list" should "be Some(Nil)" in {
    sequence1(Nil) should be (Some(Nil))
  }

  "sequence1 of List(Some(1))" should "be Some(List(1))" in {
    sequence1(List(Some(1))) should be (Some(List(1)))
  }

  "sequence1 of List(Some(1), None, Some(3))" should "be None" in {
    sequence1(List(Some(1), None, Some(3))) should be (None)
  }

  "sequence1 of List(Some(1), Some(2), Some(3))" should "be None" in {
    sequence1(List(Some(1), Some(2), Some(3))) should be (Some(List(1, 2, 3)))
  }
}
