import Chapter11._

object Chapter13 {
  sealed trait Free[F[_],A] {
    def flatMap[B](f: A => Free[F,B]): Free[F,B] = FlatMap(this, f)
    def map[B](f: A => B): Free[F,B] = flatMap(f andThen (Return(_)))
  }

  case class Return[F[_],A](a: A) extends Free[F,A]
  case class Suspend[F[_],A](s: F[A]) extends Free[F,A]
  case class FlatMap[F[_],A,B](s: Free[F,A], f: A => Free[F,B]) extends Free[F,B]

  def freeMonad[F[_]]: Monad[({type f[a] = Free[F,a]})#f] = {
    new Monad[({type f[a] = Free[F,a]})#f] {
      def flatMap[A,B](ma: Free[F,A])(f: A => Free[F,B]): Free[F,B] = ma flatMap f
      def unit[A](a: => A): Free[F,A] = Return(a)
    }
  }

  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0,A]): A = a match {
    case Return(a) => a
    case Suspend(s) => s()
    case FlatMap(x, f) => x match {
      case Return(a) => runTrampoline(f(a))
      case Suspend(s) => runTrampoline(f(s()))
      case FlatMap(y, g) => runTrampoline(y flatMap (a => g(a) flatMap f))
    }
  }

  @annotation.tailrec
  def step[F[_],A](a: Free[F,A]): Free[F,A] = a match {
    case FlatMap(FlatMap(x,f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => a
  }

  /*
  def step0[F[_],A](a: Free[F,A]): Free[F,A] = a match {
    case FlatMap(Return(x), f) => f(x)
  }
  */

  def run[F[_],A](a: Free[F,A])(implicit F: Monad[F]): F[A] = step(a) match {
    case Return(a) => F.unit(a)
    case Suspend(r) => r
    case FlatMap(x, f) => x match {
      case Suspend(r) => F.flatMap(r)(a => run(f(a)))
      case _ => sys.error("Impossible: `step` eliminates these cases")
    }
  }

  trait Translate[F[_],G[_]] { def apply[A](f: F[A]): G[A] }
  type ~>[F[_],G[_]] = Translate[F,G]

  def runFree[F[_],G[_],A](free: Free[F,A])(t: F ~> G)(implicit G: Monad[G]): G[A] = {
    step(free) match {
      case Return(a) => G.unit(a)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("Impossible: `step` eliminates these cases")
    }
  }

  def translate[F[_],G[_],A](f: Free[F,A])(fg: F ~> G): Free[G,A] = {
    type H[A] = Free[G,A]
    val t = new (F ~> H) { def apply[A](a: F[A]) = Suspend(fg(a)) }
    runFree[F,H,A](f)(t)(freeMonad)
  }

  sealed trait Console[A] {
    def toThunk: () => A
  }

  val consoleToFunction0 = new (Console ~> Function0) { def apply[A](a: Console[A]) = a.toThunk }

  def identityTranslate[F[_],A]: F ~> F = new (F ~> F) { def apply[A](a: F[A]) = a }

  implicit val function0Monad = new Monad[Function0] {
    def unit[A](a: => A) = () => a
    def flatMap[A,B](a: Function0[A])(f: A => Function0[B]) =
      () => f(a())()
  }

  def runConsole[A](a: Free[Console,A]): A = runTrampoline(translate(a)(consoleToFunction0))
}

