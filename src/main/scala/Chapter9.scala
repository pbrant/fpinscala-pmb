import util.matching.Regex

object Chapter9 {
  trait Parsers[ParseError, Parser[+_]] { self =>
    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
    implicit def string(s: String): Parser[String]
    def slice[A](p: Parser[A]): Parser[String]
    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
    implicit def regex(r: Regex): Parser[String]

    def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
    def many[A](p: Parser[A]): Parser[List[A]] = map2(p, p.many)(_ :: _) or succeed(Nil)
    def succeed[A](a: A): Parser[A] = string("") map (_ => a)

    def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
      map2(p, p2)((a, b) => (a, b))

    def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
      for {
        a <- p
        b <- p2
      } yield f(a, b)

    def map[A,B](p: Parser[A])(f: A => B): Parser[B] =
      p.flatMap(a => succeed(f(a)))

    def many1[A](p: Parser[A]): Parser[List[A]] =
      p.product(p.many).map { case (a, b) => a :: b }

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
      if (n <= 0)
        succeed(Nil)
      else
        map2(p, listOfN(n-1, p))(_ :: _)

    def numWithChars(n: Int, c: Char): Parser[List[Char]] =
      regex("\\d+".r).flatMap(x => listOfN(x.toInt, char(c)))

    def ws: Parser[String] = regex("\\s+".r)

    def digits: Parser[Int] = regex("\\d+".r).map(_.toInt)

    def delimList[A,B](delim: Parser[A], p: Parser[B]): Parser[List[B]] = {
      def rest = map2(delim, p)((_, v) => v).many

      map2(p, rest)((a, b) => a :: b) | succeed(Nil)
    }

    def __ws[A](p: Parser[A]): Parser[A] =
      for {
        _ <- ws.many
        v <- p
        _ <- ws.many
      } yield v

    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
      def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
      def map[B](f: A => B): Parser[B] = self.map(p)(f)
      def product[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)
      def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
      def many: Parser[List[A]] = self.many(p)
      def flatMap[B](f: A => Parser[B]) = self.flatMap(p)(f)
    }
  }

  sealed trait JSON
  object JSON {
    case object JNull extends JSON
    case class JNumber(get: Double) extends JSON
    case class JString(get: String) extends JSON
    case class JBool(get: Boolean) extends JSON
    case class JArray(get: IndexedSeq[JSON]) extends JSON
    case class JObject(get: Map[String, JSON]) extends JSON
  }

  trait JsonParser[ParseError, Parser[+_]] extends Parsers[ParseError, Parser] {
    import JSON._

    def __null: Parser[JNull.type] = string("null") map (_ => JNull)
    def boolTrue: Parser[JBool] = string("true") map (_ => JBool(true))
    def boolFalse: Parser[JBool] = string("false") map (_ => JBool(false))

    def digits19: Parser[Int] = regex("[1-9]\\d*".r).map(_.toInt)

    def exponent: Parser[Int] = for {
      _ <- (char('e') | char('E'))
      sign <- expSign
      e <- digits
    } yield sign*e

    def expSign: Parser[Int] = char('+').map(_ => 1) | char('-').map(_ => -1) | succeed(1)

    private def toDouble(num: Int, frac: Int): Double = (num.toString + "." + frac.toString).toDouble

    def number: Parser[JNumber] = {
      for {
          sign <- char('-').map(_ => -1) | succeed(1)
          num <- char('0').map(_ => 0) | digits19
          _ <- char('.')
          frac <- digits | succeed(0)
          exp <- exponent | succeed(1)
      } yield JNumber(math.pow(sign.toDouble * toDouble(num, frac), exp))
    }

    def __object: Parser[JObject] = {
      for {
        _ <- char('{')
        parts <- delimList(char(','), field)
        _ <- char('}')
      } yield JObject(parts.toMap)
    }

    def field: Parser[(String, JSON)] = {
      for {
        k <- __ws(__string)
        _ <- char(':')
        v <- __ws(value)
      } yield k -> v
    }

    def array: Parser[JArray] = {
      for {
        _ <- char('[')
        parts <- delimList(char(','), __ws(value))
        _ <- char(']')
      } yield JArray(parts.toIndexedSeq)
    }

    def value: Parser[JSON] =
      string | number | __object | array | boolTrue | boolFalse | __null

    def __string: Parser[String] = {
      for {
        _ <- char('\"')
        parts <- (stringChars | stringEscape).many
        _ <- char('\"')
      } yield {
        parts.mkString
      }
    }

    def string: Parser[JString] = __string.map(JString(_))

    def stringChars: Parser[String] = regex("""[^\"]+""".r)

    def stringEscape: Parser[String] =
      for {
        _ <- char('\\')
        c <- stringEscapeChar
      } yield c.toString

    def unicodeChar: Parser[Char] = regex("u[a-fA-F0-9]{4}".r).map(s =>
      java.lang.Integer.parseInt(s.substring(1), 16).toChar)

    def stringEscapeChar: Parser[Char] =
      char('"') | char('\\') | char('/') |
        char('b').map(_ => '\b') | char('b').map(_ => '\f') |
        char('n').map(_ => '\n') | char('r').map(_ => '\r') |
        char('t').map(_ => '\t') | unicodeChar
  }
}
