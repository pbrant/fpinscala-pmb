package main.scala

object Chapter14 {
  sealed trait ST[S,A] { self =>
    protected def run(s: S): (A,S)

    def map[B](f: A => B): ST[S,B] = new ST[S,B] {
      def run(s: S) = {
        val (a, s1) = self.run(s)
        (f(a), s1)
      }
    }

    def flatMap[B](f: A => ST[S,B]): ST[S,B] = new ST[S,B] {
      def run(s: S) = {
        val (a, s1) = self.run(s)
        f(a).run(s1)
      }
    }
  }
}
