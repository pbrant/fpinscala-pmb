import fpinscala.parallelism.Nonblocking._

object Chapter10 {
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
    def zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A) = a1 andThen a2
    def zero = identity
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(a1: A, a2: A) = m.op(a2, a1)
    def zero = m.zero
  }

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid[B])(a => f(_: B, a))(z)

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldMap(as, dual(endoMonoid[B]))(f.curried)(z)

  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (v.length == 0)
      m.zero
    else if (v.length == 1)
      f(v(0))
    else {
      val (l, r) = v.splitAt(v.length / 2)
      val rl = foldMapV(l, m)(f)
      val rr = foldMapV(r, m)(f)
      m.op(rl, rr)
    }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def zero: Par[A] = Par.unit(m.zero)
    def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
  }

  // Answer does both mapping and reducing in parallel
  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m))(Par.asyncF(f))

  def ordered_?(v: IndexedSeq[Int]): Boolean = {
    def o(i: Int) = if (i == v.length-1) true else v(i) < v(i+1)
    foldMap((0 until v.length).toList, booleanAnd)(o)
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, word: Int, rStub: String) extends WC

  val wcMonoid = new Monoid[WC] {
    def zero = Stub("")
    def op(a1: WC, a2: WC) = {
      (a1, a2) match {
        case (Stub(l), Stub(r)) => Stub(l + r)
        case (Stub(l), p: Part) => p.copy(lStub = l + p.lStub)
        case (p: Part, Stub(r)) => p.copy(rStub = p.rStub + r)
        case (p1: Part, p2: Part) => {
          val extra = if (p1.rStub.nonEmpty || p2.lStub.nonEmpty) 1 else 0
          Part(p1.lStub, p1.word+p2.word+extra, p2.rStub)
        }
      }
    }
  }

  def countWords(s: String): Int =  {
    val r = foldMapV(s, wcMonoid)(c => if (c.isWhitespace) Part("", 0, "") else Stub(c.toString))
    def stubWc(s: String) = if (s.nonEmpty) 1 else 0
    r match {
      case Stub(s) => stubWc(s)
      case Part(l, w, r) => w + stubWc(l) + stubWc(r)
    }
  }

  trait Foldable[F[_]] {
    def foldRight[A,B](as: F[A])(z: B)(f: (A, B) => B): B
    def foldLeft[A,B](as: F[A])(z: B)(f: (B, A) => B): B
    def foldMap[A,B](as: F[A])(f: A => B)(m: Monoid[B]): B =
      foldLeft(as)(m.zero)((b, a) => m.op(b, f(a)))
    def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

    def toList[A](fa: F[A]): List[A] = foldRight(fa)(List[A]())(_ :: _)
  }

  object ListFoldable extends Foldable[List] {
    def foldRight[A,B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    def foldLeft[A,B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  }

  object IndexedSeqFoldable extends Foldable[IndexedSeq] {
    def foldRight[A,B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    def foldLeft[A,B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    override def foldMap[A,B](as: IndexedSeq[A])(f: A => B)(m: Monoid[B]): B = foldMapV(as, m)(f)
  }

  object StreamFoldable extends Foldable[Stream] {
    def foldRight[A,B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    def foldLeft[A,B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    override def foldMap[A,B](as: Stream[A])(f: A => B)(m: Monoid[B]): B =
      as.foldRight(m.zero)((a, b) => m.op(b, f(a)))
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object TreeFoldable extends Foldable[Tree] {
    def foldRight[A,B](as: Tree[A])(z: B)(f: (A,B) => B): B =
      as match {
        case Leaf(a) => f(a, z)
        case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
      }

    def foldLeft[A,B](as: Tree[A])(z: B)(f: (B,A) => B): B =
      as match {
        case Leaf(a) => f(z, a)
        case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
      }
  }

  object OptionFoldable extends Foldable[Option] {
    def foldRight[A,B](as: Option[A])(z: B)(f: (A,B) => B): B =
      foldLeft(as)(z)((b, a) => f(a, b))

    def foldLeft[A,B](as: Option[A])(z: B)(f: (B,A) => B): B =
      as.map(a => f(z, a)).getOrElse(z)
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A,B)] = new Monoid[(A,B)] {
    def op(v1: (A,B), v2: (A,B)) = (A.op(v1._1, v2._1),B.op(v1._2, v1._2))
    def zero = (A.zero, B.zero)
  }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A=>B] = new Monoid[A=>B] {
    def op(a1: A=>B, a2: A=>B): A => B = a => B.op(a1(a), a2(a))
    def zero: A => B = _ => B.zero
  }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K,V]] = new Monoid[Map[K,V]] {
    def zero = Map[K,V]()
    def op(a: Map[K,V], b: Map[K,V]) =
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
        acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
      }
  }

  def bag[A](as: IndexedSeq[A]): Map[A,Int] = {
    IndexedSeqFoldable.foldMap(as)(a => Map(a -> 1))(mapMergeMonoid(intAddition))
  }

  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }
}

