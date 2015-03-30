object Chapter11 /* and 12 */ {
  trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]

    def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
      (map(fab)(_._1), map(fab)(_._2))

    def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] =
      e match {
        case Left(fa) => map(fa)(Left(_))
        case Right(fb) => map(fb)(Right(_))
      }
  }

  trait Monad[F[_]] extends Applicative[F] {
    F =>

    def unit[A](a: => A): F[A]
    def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

    override def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))

    override def map[A,B](fa: F[A])(f: A => B): F[B] =
      flatMap(fa)(a => unit(f(a)))

    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
      val r = traverse(ms)(a => map2(f(a), unit(a))((_, _)))
      map(r)(_.filter(_._1).map(_._2))
    }

    def filterM1[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
      ms match {
        case Nil => unit(Nil)
        case h :: t => flatMap(f(h))(b =>
          if (!b) filterM1(t)(f)
          else map(filterM1(t)(f))(h :: _))
      }

    def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
      a => flatMap(f(a))(g)

    def flatMap1[A,B](ma: F[A])(f: A => F[B]): F[B] =
      compose((_: Unit) => ma, f)(())

    def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)

    def flatMap2[A,B](ma: F[A])(f: A => F[B]): F[B] =
      join(map(ma)(f))

    def compose1[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
      a => join(map(f(a))(g))
  }

  object Monad {
    def composeM[G[_],H[_]](G: Monad[G], H: Monad[H], T: Traverse[H]): Monad[({type f[x] = G[H[x]]})#f] =
      new Monad[({type f[x] = G[H[x]]})#f] {
        def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))

        def flatMap[A, B](fa: G[H[A]])(f: A => G[H[B]]): G[H[B]] =
          G.map(G.join(G.map(fa)(ga => T.sequence(H.map(ga)(f))(G))))(H.join)
      }

    // Canonical answer
    def composeM1[G[_],H[_]](implicit G: Monad[G], H: Monad[H], T: Traverse[H]):
        Monad[({type f[x] = G[H[x]]})#f] = new Monad[({type f[x] = G[H[x]]})#f] {
      def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))
      override def flatMap[A,B](mna: G[H[A]])(f: A => G[H[B]]): G[H[B]] =
        G.flatMap(mna)(na => G.map(T.traverse(na)(f))(H.join))
    }
  }

  import fpinscala.parallelism.Nonblocking._

  object ParMonad extends Monad[Par] {
    def unit[A](a: => A) = Par.unit(a)
    def flatMap[A,B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  import Chapter9._

  def parserMonad[P[_]](parsers: Parsers[_, P]): Monad[P] = new Monad[P] {
    def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] = parsers.flatMap(ma)(f)
    def unit[A](a: => A): P[A] = parsers.succeed(a)
  }

  val optionMonad = new Monad[Option] {
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma flatMap f
    def unit[A](a: => A): Option[A] = Some(a)
  }

  val streamMonad = new Monad[Stream] {
    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma flatMap f
    def unit[A](a: => A): Stream[A] = Stream(a)
  }

  val listMonad = new Monad[List] {
    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f
    def unit[A](a: => A): List[A] = List(a)
  }

  /*
  case class Id[A](value: A) {
    def map[B](f: A => B) = f(value)
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }

  implicit object IdMonad extends Monad[Id] {
    def unit[A](a: => A) = Id(a)
    def flatMap[A,B](ma: Id[A])(f: A => Id[B]) = ma.flatMap(f)
  }
  */

  import Chapter6._

  def stateMonad[S] = new Monad[({type f[x] = State[S,x]})#f] {
    def unit[A](a: => A): State[S,A] = State(s => (a, s))
    def flatMap[A,B](st: State[S,A])(f: A => State[S,B]): State[S,B] = st flatMap f
  }

  case class Reader[R,A](run: R => A)

  object Reader {
    def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
      def unit[A](a: => A): Reader[R,A] = Reader[R,A](_ => a)
      def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] =
        Reader(r => f(st.run(r)).run(r))
    }
  }

  trait Applicative[F[_]] extends Functor[F] {
    F =>

    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] /* =
      apply(apply(unit(f.curried))(fa))(fb)
    */

    def unit[A](a: => A): F[A]

    def map[A,B](fa: F[A])(f: A => B): F[B] =
      map2(fa, unit(()))((a, _) => f(a))

    def map1[A,B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

    def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

    def sequence[A](lma: List[F[A]]): F[List[A]] = traverse(lma)(identity)

    def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
      ofa.foldRight(unit(Map[K,V]())) { case ((k,v), r) => map2(v, r)((vv, rr) => rr + (k -> vv)) }

    def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
      sequence(List.fill(n)(ma))

    def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((_, _))

    def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((ff, aa) => ff(aa))

    def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
      apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

    def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
      apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

    def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
      new Applicative[({type f[x] = (F[x], G[x])})#f] {
        override def unit[A](a: => A) = (F.unit(a), G.unit(a))
        override def map2[A,B,C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A,B) => C): (F[C], G[C]) =
          (F.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))
      }

    def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
      new Applicative[({type f[x] = F[G[x]]})#f] {
        override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
        override def map2[A, B, C](fa: F[G[A]],fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
          F.map2(fa, fb)((ga, gb) => G.map2(ga, gb)(f))
      }
  }

  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] = Stream.continually(a)

    override def map2[A,B,C](a: Stream[A], b: Stream[B])(f: (A,B) => C): Stream[C] = a zip b map f.tupled
  }

  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    def flatMap[A, B](ma: Either[E,A])(f: A => Either[E,B]): Either[E,B] = ma.right.flatMap(f)
    def unit[A](a: => A): Either[E,A] = Right(a)
  }

  sealed trait Validation[+E, +A]

  case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
  case class Success[A](a: A) extends Validation[Nothing, A]

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    def unit[A](a: => A): Validation[E,A] = Success(a)

    override def map2[A,B,C](a: Validation[E,A], b: Validation[E,B])(f: (A,B) => C): Validation[E,C] =
      (a, b) match {
        case (Success(a), Success(b)) => unit(f(a, b))
        case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, (t1 :+ h2) ++ t2)
        case (f: Failure[_], _) => f
        case (_, f: Failure[_]) => f
      }
  }

  import Chapter10._

  type Id[A] = A

  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A,B](a: A)(f: A => B): B = f(a)
  }

  trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
    F =>

    def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
      sequence(map(fa)(f))

    def sequence[G[_]:Applicative,A](fga: F[G[A]]): G[F[A]] =
      traverse(fga)(identity)

    def map[A,B](fa: F[A])(f: A => B): F[B] =
      traverse[Id,A,B](fa)(f)(idMonad)

    def traverseS[S,A,B](fa: F[A])(f: A => State[S,B]): State[S, F[B]] =
      traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(stateMonad)

    def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
      traverseS(fa)((a: A) => (for {
        s1 <- State.get[S]
        (b, s2) = f(a, s1)
        _ <- State.set(s2)
      } yield b)).run(s)

    override def toList[A](fa: F[A]): List[A] =
      mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

    def reverse[A](fa: F[A]): F[A] =
      mapAccum(fa, toList(fa).reverse)((_, s) => (s.head, s.tail))._1

    override def foldLeft[A,B](as: F[A])(z: B)(f: (B, A) => B): B =
      mapAccum(as, z)((a, s) => ((), f(s, a)))._2

    def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])(
        G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = {
      val C = G.product(H)
      traverse[({type f[x] = (G[x], H[x])})#f,A,B](fa)(a => (f(a), g(a)))(C)
    }

    def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] =
      Traverse.compose(G, F)
  }

  object Traverse {
    def compose[G[_], F[_]](G: Traverse[G], F: Traverse[F]): Traverse[({type f[x] = F[G[x]]})#f] =
      new Traverse[({type f[x] = F[G[x]]})#f] {
        override def traverse[H[_]:Applicative,A,B](fa: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
          F.traverse(fa)(ga => G.traverse(ga)(f))
      }
  }

  case class Tree[+A](head: A, tail: List[Tree[A]])

  val listTraverse = new Traverse[List] {
    override def traverse[G[_],A,B](as: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      as.foldRight(G.unit(List[B]()))((a, fbs) => G.map2(f(a), fbs)(_ :: _))
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_],A,B](as: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      as match {
        case None => G.unit(None)
        case Some(s) => G.map(f(s))(Some(_))
      }
  }

  val treeTraverse = new Traverse[Tree] {
    F =>
    override def map[A,B](fa: Tree[A])(f: A => B): Tree[B] =
      Tree(f(fa.head), fa.tail map (t => F.map(t)(f)))
  }

  def mapTraverse[K]: Traverse[({type f[x] = Map[K, x]})#f] =
    new Traverse[({type f[x] = Map[K, x]})#f] {
      override def traverse[G[_],A,B](as: Map[K,A])(f: A => G[B])(implicit G: Applicative[G]): G[Map[K,B]] =
        as.foldRight(G.unit(Map[K,B]())) { case ((k,v), fbs) => G.map2(fbs, f(v))((m, n) => m + (k -> n)) }
    }
}
