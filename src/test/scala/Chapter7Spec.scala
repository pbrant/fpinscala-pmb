import org.scalatest._

import Chapter7._

import java.util.concurrent._

class Chapter7Spec extends FlatSpec with Matchers {

  def withEs[A](f: ExecutorService => A): A = {
    val pool = Executors.newFixedThreadPool(10)
    try {
      f(pool)
    } finally {
      pool.shutdown()
      pool.awaitTermination(1, TimeUnit.DAYS)
    }
  }

  "fold" should "be able to select maximum" in {
    val result = withEs { pool =>
      Par.run(pool)(Par.fold(Vector(1, 2, 3, 99, 5), -1)(identity)(_ max _)).get
    }
    result should be (99)
  }

  "fold" should "be able to count words" in {
    val result = withEs { pool =>
      Par.run(pool)(Par.fold(Vector(
        "a b c",
        "",
        "b d"), 0)(s => if (s.isEmpty) 0 else s.split("\\s+").length)(_ + _)).get
    }

    result should be (5)
  }
}
