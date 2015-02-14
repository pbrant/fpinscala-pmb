object Chapter5 {
  sealed trait Stream[+A] {
    import Stream._

    def toList: List[A] = {
      this match {
        case Cons(h, t) => h() :: t().toList
        case _ => Nil
      }
    }

    def take(n: Int): Stream[A] = {
      this match {
        case Cons(h, t) => {
          if (n <= 0)
            empty
          else if (n == 1)
            cons(h(), empty)
          else
            cons(h(), t().take(n-1))
        }
        case _ => empty
      }
    }

    def take1(n: Int): Stream[A] = {
      unfold((n, this)) { case (_n, s) =>
        s match {
          case Cons(h, t) => if (n <= 0) None else Some((h(), (n-1, t())))
          case _ => None
        }
      }
    }

    def takeWhile(p: A => Boolean): Stream[A] = {
      this match {
        case Cons(h, t) => {
          val v = h()
          if (p(v))
            cons(v, t().takeWhile(p))
          else
            empty
        }
        case _ => empty
      }
    }

    def takeWhile2(p: A => Boolean): Stream[A] = {
      unfold(this) { case s =>
        s match {
          case Cons(h, t) => if (p(h())) Some(h(), t()) else None
          case _ => None
        }
      }
    }

    def drop(n: Int): Stream[A] = {
      this match {
        case Cons(h, t) => {
          if (n <= 0)
            this
          else if (n == 1)
            t()
          else
            t().drop(n-1)
        }
        case _ => this
      }
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = {
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => if (! p(a)) false else b)

    def takeWhile1(p: A => Boolean): Stream[A] =
      foldRight(empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else empty)

    def headOption: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))

    def map[B](f: A => B): Stream[B] =
      foldRight(empty: Stream[B])((a, b) => cons(f(a), b))

    def map1[B](f: A => B): Stream[B] = {
      unfold(this)(s => s match {
        case Cons(h, t) => Some((f(h()), t()))
        case _ => None
      })
    }

    def filter(f: A => Boolean): Stream[A] =
      foldRight(empty: Stream[A])((a, b) => if (f(a)) cons(a, b) else b)

    /*
    def append[B >: A](b: => B): Stream[B] =
      foldRight(Stream(b))((a, b) => cons(a, b))
    */
    def append[B >: A](b: => Stream[B]): Stream[B] =
      foldRight(b)((a, b) => cons(a, b))

    def flatMap[B >: A](f: A => Stream[B]): Stream[B] =
      foldRight(empty: Stream[B])((a, b) => f(a).foldRight(b)((a2, b2) => cons(a2, b2)))

    def zipWith[A1 >: A, B](a1: Stream[A1])(f: (A1, A1) => B): Stream[B] = {
      unfold((this, a1)) { case (_a1, _a2) =>
        (_a1, _a2) match {
          case (Empty, _) | (_, Empty) => None
          case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
        }
      }
    }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
      unfold((this, s2)) { case (_a1, _a2) =>
        def next[C](s: Stream[C]): (Option[C], Stream[C]) = {
          s match {
            case Cons(h, t) => (Some(h()), t())
            case e => (None, e)
          }
        }

        val (v1, s1) = next(_a1)
        val (v2, s2) = next(_a2)

        if (! v1.isDefined && ! v2.isDefined)
          None
        else
          Some((v1, v2), (s1, s2))
      }
    }

    def startsWith[A](s: Stream[A]): Boolean = this.zipWith(s)(_ == _).forAll(identity)

    def tails: Stream[Stream[A]] = {
      unfold(Some(this): Option[Stream[A]])(o => {
        o.map(s =>
          s match {
            case Cons(_, t) => (s, Some(t()))
            case _ => (s, None)
          }
        )
      })
    }

    // See answer for better solution (that caches intermediate solutions)
    def scanRight[B >: A](z: => B)(f: (B, => B) => B): Stream[B] = {
      foldRight[Stream[B]](Stream(z))((a, b) => {
        val v = b match {
          case Cons(h, _) => f(a, h())
          case _ => a
        }
        cons(v, b)
      })
    }
  }

  final case object Empty extends Stream[Nothing]
  final case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    def constant[A](a: A): Stream[A] =
      cons(a, constant(a))

    def from(n: Int): Stream[Int] =
      cons(n, from(n+1))

    def fibs: Stream[Int] = {
      def loop(last: Int, current: Int): Stream[Int] =
        cons(current, loop(current, last+current))
      cons(0, loop(0, 1))
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z).map(p => cons(p._1, unfold(p._2)(f))).getOrElse(empty)

    def constant1[A](a: A): Stream[A] =
      unfold(a)(a => Some((a, a)))

    def from1[A](n: Int): Stream[Int] =
      unfold(n)(a => Some((a, a+1)))

    def fibs1: Stream[Int] =
      unfold((0, 1)) { case (current, next) => Some((current, (next, current+next))) }
  }
}
