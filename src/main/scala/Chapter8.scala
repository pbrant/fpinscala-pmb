import Chapter6._

object Chapter8 {
  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String
  type MaxSize = Int

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace\n ${e.getStackTrace.mkString("\n")}"

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    unfoldStream(g.sample)(rng)

  def unfoldStream[S, A](gen: State[S, A])(s: S): Stream[A] = {
    val (a, s2) = gen.run(s)
    a #:: unfoldStream(gen)(s2)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop{ (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def run(
    p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = SimpleRNG(System.currentTimeMillis)): Result = {
    val result = p.run(maxSize, testCases, rng) 
    result match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }
    result
  }

  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
    def &&(p: Prop): Prop = {
      Prop((m, c, r) => {
          this.run(m, c, r) match {
            case Passed => p.run(m, c, r)
            case f => f
          }
      })
    }

    def ||(p: Prop): Prop = {
      Prop((m, c, r) => {
          this.run(m, c, r) match {
            case Passed => Passed
            case _ => p.run(m, c, r)
          }
        })
    }
  }

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }
}

case class Gen[+A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(a => f(a)))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(s => Gen.listOfN(s, this))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean.flatMap(tf => if (tf) g1 else g2)

  def unsized: SGen[A] = SGen(_ => this)
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(i => forSize(i).flatMap(a => f(a).forSize(i)))

  def listOfN(size: SGen[Int]): SGen[List[A]] =
    SGen(i => forSize(i).listOfN(size.forSize(i)))

  def union[A](g1: SGen[A], g2: SGen[A]): SGen[A] =
    Gen.boolean.unsized.flatMap(tf => if (tf) g1 else g2)
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(nonNegativeLessThan2(stopExclusive-start).map(_ + start))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(int.map(_ % 2 == 0))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(i => listOfN(i, g))

  def listOf1[A](g: Gen[A]) =
    SGen(i => listOf(g).forSize(i+1))

  def pair[A](g: Gen[A]): Gen[(A, A)] = Gen(g.sample.map2(g.sample)((_, _)))

  def string(n: Int): Gen[String] =
    Gen(listOfN(n, choose('A', 'Z')).sample.map(
      a => new String(a.map(_.toChar).toArray)))
}
