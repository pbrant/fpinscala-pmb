import org.scalatest._

import Chapter8._

import java.util.concurrent._

class Chapter8Spec extends FlatSpec with Matchers {
  "simple example" should "work" in {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = forAll(Gen.listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }

    Chapter8.run(maxProp) should be (Passed)
  }

  "sorted" should "work" in {
    val smallInt = Gen.choose(-10, 10)
    def prop1(f: List[Int] => Boolean) =
      forAll(Gen.listOf1(smallInt))(f)
    val minFirst = prop1 { ns => ns.sorted.min == ns.sorted.head }
    val maxLast = prop1 { ns => ns.sorted.max == ns.sorted.last }
    Chapter8.run(minFirst && maxLast) should be (Passed)
  }
}
