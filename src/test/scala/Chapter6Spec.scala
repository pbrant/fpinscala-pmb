import org.scalatest._

import Chapter6._

class Chapter6Spec extends FlatSpec with Matchers {
  "ints and ints2" should "produce the same results" in {
    val rng = SimpleRNG(42)
    ints(10)(rng) should be (ints2(10)(rng))
  }

  import Chapter6.Candy._
  "Four valid purchases" should "increase coins by four and decrease candies by four" in {
    val input = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    val ((coins, candies), _) = simulateMachine(input).run(Machine(true, 5, 10))
    (coins, candies) should be ((14, 1))
  }

  "Candy test #1" should "produce correct results" in {
    val input = List(Coin, Coin, Turn)
    val ((coins, candies), _) = simulateMachine(input).run(Machine(true, 5, 10))
    (coins, candies) should be ((11, 4))
  }

  "Candy test #2" should "produce correct results" in {
    val input = List(Coin, Turn)
    val ((coins, candies), _) = simulateMachine(input).run(Machine(true, 0, 0))
    (coins, candies) should be ((0, 0))
  }

  "Candy test #3" should "produce correct results" in {
    val input = List(Turn, Turn)
    val ((coins, candies), _) = simulateMachine(input).run(Machine(true, 10, 10))
    (coins, candies) should be ((10, 10))
  }
}
