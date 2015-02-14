object Chapter3 extends App {
  def tail[A](list: List[A]): List[A] = {
    list match {
      case Nil => sys.error("Nil has no tail")
      case h :: tail => tail
    }
  }

  def setHead[A](a: A, list: List[A]): List[A] = {
    list match {
      case Nil => List(a)
      case _ => a :: tail(list)
    }
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0 || l.isEmpty)
      l
    else
      drop(tail(l), n-1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => l
      case r@(head :: tail) => {
        if (f(head))
          dropWhile(tail, f)
        else
          r
      }
    }
  }

  def init[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def init0(r: List[A], l: List[A]): List[A] = {
      l match {
        case Nil => Nil
        case head :: Nil => r.reverse
        case head :: tail => init0(head :: r, tail)
      }
    }

    init0(Nil, l)
  }

  def length0[A](l: List[A]): Int = {
    l.foldRight(0)((_, c) => c + 1)
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case head :: tail => foldLeft(tail, f(z, head))(f)
    }
  }

  def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)
  def product(ints: List[Int]): Int = foldLeft(ints, 1)(_ * _)
  def length1[A](as: List[A]): Int = foldLeft(as, 0)((b, _) => b + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((r, a) => a :: r)

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((b, a) => f(a, b))
  }

  def append[A](as: List[A], a: A): List[A] = {
    foldRight(as, List(a))(_ :: _)
  }

  def concat[A](lists: List[List[A]]): List[A] = {
    foldRight(lists, Nil: List[A])((as, r) => foldRight(as, r)((a, b) => a :: r))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    @annotation.tailrec
    def loop(o: List[A], r: List[B]): List[B] = {
      o match {
        case Nil => reverse(r)
        case head :: tail => loop(tail, f(head) :: r)
      }
    }

    loop(as, Nil)
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    @annotation.tailrec
    def loop(o: List[A], r: List[A]): List[A] = {
      o match {
        case Nil => reverse(r)
        case head :: tail => {
          if (f(head))
            loop(tail, head :: r)
          else
            loop(tail, r)
        }
      }
    }

    loop(as, Nil)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def filter1[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) List(a) else Nil)
  }

  def zipWith[A](a1s: List[A], a2s: List[A])(f: (A, A) => A): List[A] = {
    @annotation.tailrec
    def loop(_a1s: List[A], _a2s: List[A], r: List[A]): List[A] = {
      (_a1s, _a2s) match {
        case (_, Nil) | (Nil, _) => r.reverse
        case (a1head :: a1tail, a2head :: a2tail) => 
          loop(a1tail, a2tail, f(a1head, a2head) :: r)
      }
    }

    loop(a1s, a2s, Nil)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    val l = sub.length

    @annotation.tailrec
    def loop(s: List[A]): Boolean = {
      if (s.drop(l) == sub)
        true
      else if (s.nonEmpty)
        loop(s.tail)
      else
        false
    }

    loop(sup)
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }

  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }

  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + depth(l).max(depth(r))
    }
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A, B](tree: Tree[A])(f1: A => B)(f2: (B, B) => B): B = {
    tree match {
      case Leaf(v) => f1(v)
      case Branch(l, r) => f2(fold(l)(f1)(f2), fold(r)(f1)(f2))
    }
  }

  def size1[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((l, r) => 1 + l + r)
  def maximum1[A](tree: Tree[Int]): Int = fold(tree)(v => v)((l, r) => l.max(r))
  def depth1[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((l, r) => 1 + l.max(r))
  def map1[A, B](tree: Tree[A])(f: A => B): Tree[B] = 
    fold(tree)(v => Leaf(f(v)): Tree[B])((l, r) => Branch(l, r))
}
