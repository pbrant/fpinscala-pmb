object Chapter2 extends App {
  def isSorted[A](as: List[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def isSorted0(as0: List[A]): Boolean = {
      as0 match {
        case Nil | _ :: Nil => true
        case first :: second :: rest => {
          if (! ordered(first, second))
            false
          else 
            isSorted0(second :: rest)
        }
      }
    }

    isSorted0(as)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 2) 
        true
      else if (! ordered(as(n), as(n+1)))
        false
      else
        loop(n+1)
    }

    loop(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => (b => f(a, b))
  }

    def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
