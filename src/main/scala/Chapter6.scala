object Chapter6 {
  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (r, next) = rng.nextInt
    (if (r < 0) math.abs(r+1) else r, next)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (r, next) = nonNegativeInt(rng)
    (r.toDouble / (Int.MaxValue.toDouble+1), next)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, next) = nonNegativeInt(rng)
    val (d, ret) = double(next)
    ((i,d), ret)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i,d), ret) = intDouble(rng)
    ((d,i), ret)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def loop(c: Int, l: List[Int], r: RNG): (List[Int], RNG) = {
      if (c <= 0)
        (l, r)
      else {
        val (i, n) = r.nextInt
        loop(c-1, i :: l, n)
      }
    }

    loop(count, Nil, rng)
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i  % 2)

  def double2(rng: RNG): Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue.toDouble+1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def sequence[A](fa: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      fa.foldRight((Nil: List[A], rng))((ra, result) => {
        val (l, r) = result
        val (a, r2) = ra(r)
        (a :: l, r2)
      })
    }
  }

  // Canonical version
  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
   fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence[Int](List.fill(count)(_.nextInt))(rng)

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, r) = f(rng)
      g(a)(r)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    })

  def map12[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map22[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  case class State[S, +A](run: S => (A, S)) {
    def flatMap[B](g: A => State[S, B]): State[S, B] = {
      State[S, B](
        s => {
          val (a, s2) = run(s)
          g(a).run(s2)
        }
      )
    }

    def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

    def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => rb.map(b => f(a, b)))
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State[S, A](s => (a, s))

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
      // See alternatives in answers (reverse + foldLeft, etc)
      fs.foldRight(unit[S, List[A]](List[A]()))((f, acc) => f.map2(acc)(_ :: _))
    }

    def get[S]: State[S, S] = State(s => (s, s))
    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
    def modify[S](f: S => S): State[S, Unit] = 
      for {
        s <- get
        _ <- set(f(s))
      } yield ()
  }

  type Rand2[A] = State[RNG, A]

  def int: Rand2[Int] = State[RNG, Int](_.nextInt)

  def nonNegativeInt: Rand2[Int] =
    int.map(i => if (i < 0) math.abs(i+1) else i)

  def nonNegativeLessThan2(n: Int): Rand2[Int] =
    nonNegativeInt.flatMap(i => {
      val mod = i % n
      if (i + (n-1) - mod >= 0) State.unit(mod) else nonNegativeLessThan2(n)
    })

  def ints3(count: Int): Rand2[List[Int]] =
    State.sequence[RNG, Int](List.fill(count)(int))

  object Candy {
    sealed trait Input
    case object Coin extends Input
    case object Turn extends Input
    
    case class Machine(locked: Boolean, candies: Int, coins: Int)

    def respondToCoin(machine: Machine): Machine = {
      machine match {
        case Machine(_, 0, _) | Machine(false, _, _) => machine
        case Machine(true, candies, coins) => Machine(false, candies, coins+1)
      }
    }

    def respondToTurn(machine: Machine): Machine = {
      machine match {
        case Machine(_, 0, _) | Machine(true, _, _) => machine
        case Machine(false, candies, coins) => Machine(true, candies-1, coins) 
      }
    }

    def respondToInput(machine: Machine, input: Input): Machine = {
      input match {
        case Coin => respondToCoin(machine)
        case Turn => respondToTurn(machine)
      }
    }

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
      inputs
        .foldLeft(State.get[Machine])((s, i) => s.map(respondToInput(_, i)))
        .map(m => (m.coins, m.candies))
    }

    import State._

    // Canonical answer was this
    def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] = for {
      _ <- State.sequence(inputs.map(i => State.modify((s: Machine) => (i, s) match {
        case (_, Machine(_, 0, _)) => s
        case (Coin, Machine(false, _, _)) => s
        case (Turn, Machine(true, _, _)) => s
        case (Coin, Machine(true, candy, coin)) =>
          Machine(false, candy, coin + 1)
        case (Turn, Machine(false, candy, coin)) =>
          Machine(true, candy - 1, coin)
      })))
      s <- State.get
    } yield (s.coins, s.candies)

    def simulateMachine3(inputs: List[Input]): State[Machine, (Int, Int)] = 
      State.sequence(inputs.map(i => State.modify((s: Machine) => (i, s) match {
        case (_, Machine(_, 0, _)) => s
        case (Coin, Machine(false, _, _)) => s
        case (Turn, Machine(true, _, _)) => s
        case (Coin, Machine(true, candy, coin)) =>
          Machine(false, candy, coin + 1)
        case (Turn, Machine(false, candy, coin)) =>
          Machine(true, candy - 1, coin)
      }))).flatMap(_ => State.get.map(s => (s.coins, s.candies)))
  }

}
