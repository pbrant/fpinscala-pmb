import org.scalatest._

import Chapter3._

class Chapter3Spec extends FlatSpec with Matchers {
  "tail of empty List" should "throw exception" in {
    a [RuntimeException] should be thrownBy {
      tail(Nil)
    }
  }

  "tail of [1, 2, 3]" should "be [2, 3]" in {
    tail(List(1, 2, 3)) should be (List(2, 3))
  }

  "setHead of Nil" should "be the new head" in {
    setHead(1, Nil) should be (List(1))
  }

  "setHead 99 of [1, 2, 3]" should "be [99, 2, 3]" in {
    setHead(99, List(1, 2, 3)) should be (List(99, 2, 3))
  }

  "drop on Nil" should "be Nil" in {
    drop(Nil, 99) should be (Nil)
  }

  "drop 2 on [1, 2, 3]" should "be [3]" in {
    drop(List(1, 2, 3), 2) should be (List(3))
  }

  "dropWhile true on Nil" should "be Nil" in {
    dropWhile[Int](Nil, _ => true) should be (Nil)
  }

  "dropWhile less than 3 on [1, 2, 3]" should "be [3]" in {
    dropWhile[Int](List(1, 2, 3), _ < 3) should be (List(3))
  }

  "init on [1, 2, 3, 4]" should "[2, 3, 4]" in {
    init(List(1, 2, 3, 4)) should be (List(1, 2, 3))
  }

  "length on [1, 2, 3]" should "be 3" in {
    length0(List(1, 2, 3)) should be (3)
  }

  "length1 on [1, 2, 3]" should "be 3" in {
    length1(List(1, 2, 3)) should be (3)
  }

  "reverse on [1, 2, 3]" should "be [3, 2, 1]" in {
    reverse(List(1, 2, 3)) should be (List(3, 2, 1))
  }

  "append 4 on [1, 2, 3]" should "be [1, 2, 3, 4]" in {
    append(List(1, 2, 3), 4) should be (List(1, 2, 3, 4))
  }

  "concat [1, 2] with [3, 4]" should "be [1, 2, 3, 4]" in {
    concat(List(List(1, 2), List(3, 4)))
  }

  "add 1 to [1, 2, 3]" should "be [2, 3, 4]" in {
    map(List(1, 2, 3))(_ + 1) should be (List(2, 3, 4))
  }

  "filter even numbers on [1, 2, 3, 4]" should "be [2, 4]" in {
    filter(List(1, 2, 3, 4))(_ % 2 == 0) should be (List(2, 4))
  }

  "filter1 even numbers on [1, 2, 3, 4]" should "be [2, 4]" in {
    filter1(List(1, 2, 3, 4))(_ % 2 == 0) should be (List(2, 4))
  }

  "zipWith [1, 2, 3] + [4, 5, 6]" should "be [5, 7, 9]" in {
    zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) should be (List(5, 7, 9))
  }

  "hasSubsequence([1, 2, 3, 4], [3, 4])" should "be true" in {
    hasSubsequence(List(1, 2, 3, 4), List(3, 4)) should be (true)
  }

  "hasSubsequence([1, 2, 3, 4], [5, 4])" should "be false" in {
    hasSubsequence(List(1, 2, 3, 4), List(5, 4)) should be (false)
  }
}
