object Chapter4 extends App {
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B]
    def flatMap[B](f: A => Option[B]): Option[B]
    def getOrElse[B >: A](default: => B): B
    def orElse[B >: A](ob: => Option[B]): Option[B]
    def filter(f: A => Boolean): Option[A]
  }

  case class Some[+A](get: A) extends Option[A] {
    def map[B](f: A => B): Option[B] = Some(f(get))
    def flatMap[B](f: A => Option[B]): Option[B] = f(get)
    def getOrElse[B >: A](default: => B): B = get
    def orElse[B >: A](ob: => Option[B]): Option[B] = this
    def filter(f: A => Boolean): Option[A] =
      if (f(get)) this else None
  }

  case object None extends Option[Nothing] {
    def map[B](f: Nothing => B): Option[B] = this
    def flatMap[B](f: Nothing => Option[B]): Option[B] = this
    def getOrElse[B](default: => B): B = default
    def orElse[B](ob: => Option[B]): Option[B] = ob
    def filter(f: Nothing => Boolean): Option[Nothing] = this
  }

  // ????
  def variance(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty)
      None
    else {
      val m = xs.sum / xs.length
      val v = xs.map(x => math.pow(x - m, 2)).sum / xs.length
      Some(v)
    }
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)
  }

  def sequence[A](as: List[Option[A]]): Option[List[A]] = {
    val b: Option[List[A]] = Some(Nil)
    as.foldRight(b)((a, r) => map2(a, r)((aa, rr) => aa :: rr))
  }

  def traverseO[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    val b: Option[List[B]] = Some(Nil)
    as.foldRight(b)((a, r) => map2(f(a), r)((bb, rr) => bb :: rr))
  }

  def sequence1[A](as: List[Option[A]]): Option[List[A]] = 
    traverseO(as)(identity)

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B]
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
  }

  case class Left[+E](value: E) extends Either[E, Nothing] {
    def map[B](f: Nothing => B): Either[E, B] = this
    def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = this
    def orElse[EE >: E, B](b: => Either[EE, B]): Either[EE, B] = b
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = this
  }

  case class Right[+A](value: A) extends Either[Nothing, A] {
    def map[B](f: A => B): Either[Nothing, B] = Right(f(value))
    def flatMap[EE, B](f: A => Either[EE, B]): Either[EE, B] = f(value)
    def orElse[EE, B >: A](b: => Either[EE, B]): Either[EE, B] = this
    def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
      b.flatMap(bb => Right(f(value, bb))) 
  }

  def traverseE[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    val b: Either[E, List[B]] = Right(Nil)
    as.foldRight(b)((a, r) => r.map2(f(a))((rr, bb) => bb :: rr))
  }

  def sequenceE[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverseE(es)(identity)
}
