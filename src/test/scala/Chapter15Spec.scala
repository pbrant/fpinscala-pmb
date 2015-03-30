import org.scalatest._

import Chapter15._
import Process._

class Chapter15Spec extends FlatSpec with Matchers {
  "take(0)(Stream(1,2,3,4)).toList" should "be List()" in {
    take(0)(Stream(1,2,3,4)).toList should be (List())
  }

  "take(1)(Stream(1,2,3,4)).toList" should "be List(1)" in {
    take(1)(Stream(1,2,3,4)).toList should be (List(1))
  }

  "take(10)(Stream(1,2,3,4)).toList" should "be List(1,2,3,4)" in {
    take(10)(Stream(1,2,3,4)).toList should be (List(1,2,3,4))
  }

  "drop(0)(Stream(1,2,3)).toList" should "be List(1,2,3)" in {
    drop(0)(Stream(1,2,3)).toList should be (List(1,2,3))
  }

  "drop(1)(Stream(1,2,3)).toList" should "be List(2,3)" in {
    drop(1)(Stream(1,2,3)).toList should be (List(2,3))
  }

  "drop(99)(Stream(1,2,3)).toList" should "be List()" in {
    drop(99)(Stream(1,2,3)).toList should be (List())
  }

  "dropWhile(_ <= 3)(Stream(1,2,3,4)).toList" should "be List(4)" in {
    dropWhile[Int](_ <= 3)(Stream(1,2,3,4)).toList should be (List(4))
  }

  "dropWhile(_ <= 0)(Stream(1,2,3,4)).toList" should "be List(1,2,3,4)" in {
    dropWhile[Int](_ <= 0)(Stream(1,2,3,4)).toList should be (List(1,2,3,4))
  }

  "takeWhile(_ <= 0)(Stream(1,2,3,4)).toList" should "be List(1,2,3,4)" in {
    takeWhile[Int](_ <= 0)(Stream(1,2,3,4)).toList should be (List())
  }

  "takeWhile(_ <= 3)(Stream(1,2,3)).toList" should "be List(1,2,3)" in {
    takeWhile[Int](_ <= 3)(Stream(1,2,3,4)).toList should be (List(1,2,3))
  }

  "count(Stream(1,2,3,4)).toList" should "be List(1,2,3,4)" in {
    count(Stream(1,2,3,4)).toList should be (List(0,1,2,3))
  }

  "count1(Stream(1,2,3,4)).toList" should "be List(1,2,3,4)" in {
    count1(Stream(1,2,3,4)).toList should be (List(0,1,2,3))
  }

  "mean(Stream(1,2,3,4)).toList" should "be List(1,1.5,2,2.5)" in {
    mean(Stream(1,2,3,4)).toList should be (List(1,1.5,2,2.5))
  }

  "sum(Stream(1,2,3,4)).toList" should "be List(1,3,6,10)" in {
    sum(Stream(1,2,3,4)).toList should be (List(1,3,6,10))
  }
}
