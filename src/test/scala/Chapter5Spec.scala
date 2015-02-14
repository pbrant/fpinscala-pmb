import org.scalatest._

import Chapter5._

class Chapter5Spec extends FlatSpec with Matchers {
  "Stream(1, 2, 3).toList" should "be [1, 2, 3]" in {
    Stream(1, 2, 3).toList should be (List(1, 2, 3))
  }

  "Stream.empty.take(99).toList" should "be Nil" in {
    Stream.empty.take(99).toList should be (Nil)
  }

  "Stream(1, 2, 3).take(3).toList" should "be [1, 2, 3]" in {
    Stream(1, 2, 3).take(3).toList should be (List(1, 2, 3))
  }

  "Stream(1, 2, 3).take(99).toList" should "be [1, 2, 3]" in {
    Stream(1, 2, 3).take(99).toList should be (List(1, 2, 3))
  }

  "Stream(1, 2, 3).take(0).toList" should "be Nil" in {
    Stream(1, 2, 3).take(0).toList should be (Nil)
  }

  "Stream.empty.take1(99).toList" should "be Nil" in {
    Stream.empty.take1(99).toList should be (Nil)
  }

  "Stream(1, 2, 3).take1(3).toList" should "be [1, 2, 3]" in {
    Stream(1, 2, 3).take1(3).toList should be (List(1, 2, 3))
  }

  "Stream(1, 2, 3).take1(99).toList" should "be [1, 2, 3]" in {
    Stream(1, 2, 3).take1(99).toList should be (List(1, 2, 3))
  }

  "Stream(1, 2, 3).take1(0).toList" should "be Nil" in {
    Stream(1, 2, 3).take1(0).toList should be (Nil)
  }

  "Stream.empty.drop(99).toList" should "be Nil" in {
    Stream.empty.drop(99).toList should be (Nil)
  }

  "Stream(1, 2, 3).drop(2).toList" should "be [3]" in {
    Stream(1, 2, 3).drop(2).toList should be (List(3))
  }

  "Stream(1, 2, 3).drop(0).toList" should "be [1, 2, 3]" in {
    Stream(1, 2, 3).drop(0).toList should be (List(1, 2, 3))
  }

  "Stream(1, 2, 3).drop(99).toList" should "be Nil" in {
    Stream(1, 2, 3).drop(99).toList should be (Nil)
  }

  "Stream(1, 2, 3).takeWhile(_ < 3).toList" should "be List(1, 2)" in {
    Stream(1, 2, 3).takeWhile(_ < 3).toList should be (List(1, 2))
  }

  "Stream(1, 2, 3).takeWhile(_ < 99).toList" should "be List(1, 2, 3)" in {
    Stream(1, 2, 3).takeWhile(_ < 99).toList should be (List(1, 2, 3))
  }

  "Stream(1, 2, 3).takeWhile1(_ < 3).toList" should "be List(1, 2)" in {
    Stream(1, 2, 3).takeWhile1(_ < 3).toList should be (List(1, 2))
  }

  "Stream(1, 2, 3).takeWhile1(_ < 99).toList" should "be List(1, 2, 3)" in {
    Stream(1, 2, 3).takeWhile1(_ < 99).toList should be (List(1, 2, 3))
  }

  "Stream(1, 2, 3).takeWhile2(_ < 3).toList" should "be List(1, 2)" in {
    Stream(1, 2, 3).takeWhile2(_ < 3).toList should be (List(1, 2))
  }

  "Stream(1, 2, 3).takeWhile2(_ < 99).toList" should "be List(1, 2, 3)" in {
    Stream(1, 2, 3).takeWhile2(_ < 99).toList should be (List(1, 2, 3))
  }

  "Stream.empty.forAll((_: Nothing) => true)" should "be true" in {
    Stream.empty.forAll((_: Nothing) => true) should be (true)
  }

  "Stream(1, 2, 3).forAll(_ < 99)" should "be true" in {
    Stream(1, 2, 3).forAll(_ < 99) should be (true)
  }

  "Stream(1, 2, 3).forAll(_ < 2)" should "be false" in {
    Stream(1, 2, 3).forAll(_ < 2) should be (false)
  }

  "Stream.empty.headOption" should "be None" in {
    Stream.empty.headOption should be (None)
  }

  "Stream(1, 2, 3).headOption" should "be Some(1)" in {
    Stream(1, 2, 3).headOption should be (Some(1))
  }

  "Stream(1, 2, 3).map(_ + 1).toList" should "be [2, 3, 4]" in {
    Stream(1, 2, 3).map(_ + 1).toList should be (List(2, 3, 4))
  }

  "Stream(1, 2, 3).map1(_ + 1).toList" should "be [2, 3, 4]" in {
    Stream(1, 2, 3).map1(_ + 1).toList should be (List(2, 3, 4))
  }

  "Stream(1, 2, 3).filter(_ % 2 == 0).toList" should "be [2]" in {
    Stream(1, 2, 3).filter(_ % 2 == 0).toList should be (List(2))
  }

  "Stream(1, 2, 3).append(Stream(4)).toList" should "be [1, 2, 3, 4]" in {
    Stream(1, 2, 3).append(Stream(4)).toList should be (List(1, 2, 3, 4))
  }

  "Stream(1, 2, 3).flatMap(x => Stream(x, x)).toList" should "be [1, 1, 2, 2, 3, 3]" in {
    Stream(1, 2, 3).flatMap(x => Stream(x, x)).toList should be (List(1, 1, 2, 2, 3, 3))
  }

  "Stream.constant(5).take(5).toList" should "be [5, 5, 5, 5, 5]" in {
    Stream.constant(5).take(5).toList should be (List(5, 5, 5, 5, 5))
  }

  "Stream.constant1(5).take(5).toList" should "be [5, 5, 5, 5, 5]" in {
    Stream.constant1(5).take(5).toList should be (List(5, 5, 5, 5, 5))
  }

  "Stream.from(10).take(2).toList" should "be [10, 11]" in {
    Stream.from(10).take(2).toList should be (List(10, 11))
  }

  "Stream.from1(10).take(2).toList" should "be [10, 11]" in {
    Stream.from1(10).take(2).toList should be (List(10, 11))
  }

  "Stream.fibs.take(7).toList" should "be [0, 1, 1, 2, 3, 5, 8]" in {
    Stream.fibs.take(7).toList should be (List(0, 1, 1, 2, 3, 5, 8))
  }

  "Stream.fibs1.take(7).toList" should "be [0, 1, 1, 2, 3, 5, 8]" in {
    Stream.fibs1.take(7).toList should be (List(0, 1, 1, 2, 3, 5, 8))
  }

  "Stream(1, 2, 3).zipWith(Stream(1, 2, 3)(_ + _).toList" should "be [2, 4, 6]" in {
    Stream(1, 2, 3).zipWith(Stream(1, 2, 3))(_ + _).toList should be (List(2, 4, 6))
  }

  "Stream(1, 2, 3).zipAll((Stream(1, 2))).toList" should "be [...]" in {
    Stream(1, 2, 3).zipAll(Stream(1, 2)).toList should be (List((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), None)))
  }

  "Stream(1, 2, 3).startsWith(Stream(1, 2))" should "be true" in {
    Stream(1, 2, 3).startsWith(Stream(1, 2)) should be (true)
  }

  "Stream(1, 2, 3).tails" should "be [...]" in {
    Stream(1, 2, 3).tails.toList.map(_.toList) should be (List(List(1, 2, 3), List(2, 3), List(3), List()))
  }

  "Stream(1, 2, 3).scanRight(0)(_ + _).toList" should "be [6, 5, 3, 0]" in {
    Stream(1, 2, 3).scanRight(0)(_ + _).toList should be (List(6, 5, 3, 0))
  }
}
