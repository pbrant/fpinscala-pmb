import org.scalacheck._
import org.scalacheck.Prop.forAll

import Chapter10._

object Chapter10Props extends Properties("Chapter10") {
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): (Prop, Prop) = {
    (
      forAll(gen, gen, gen)((x, y, z) => m.op(m.op(x, y), z) == m.op(x, m.op(y, z))),
      forAll(gen)(x => m.op(x, m.zero) == m.op(m.zero, x))
    )
  }

  def endoMonoidLaws[A](m: Monoid[A=>A], fgen: Gen[A=>A], gen: Gen[A]): (Prop, Prop) = {
    (
      forAll(fgen, fgen, fgen, gen)((x, y, z, i) => m.op(m.op(x, y), z)(i) == m.op(x, m.op(y, z))(i)),
      forAll(fgen, gen)((x, i) => m.op(x, m.zero)(i) == m.op(m.zero, x)(i))
    )
  }

  def nameProps(s: String, p: (Prop, Prop)): Unit = {
    property(s + "Op") = p._1
    property(s + "Zero") = p._2
  }

  nameProps("intAddition", monoidLaws(Chapter10.intAddition, Gen.choose(-100, 100)))
  nameProps("intMultiplication", monoidLaws(Chapter10.intMultiplication, Gen.choose(-100, 100)))
  nameProps("booleanOr", monoidLaws(Chapter10.booleanOr, Gen.oneOf(true, false)))
  nameProps("booleanAnd", monoidLaws(Chapter10.booleanAnd, Gen.oneOf(true, false)))
  nameProps("optionMonoid", monoidLaws(
    Chapter10.optionMonoid[Int],
    Gen.choose(-100, 100).map(i => if (i < 0) None else Some(i))))
  nameProps("endoMonoid", endoMonoidLaws(
    Chapter10.endoMonoid[Int],
    Gen.oneOf(
      (i: Int) => i,
      (i: Int) => -1,
      (i: Int) => i*2,
      (i: Int) => i*i),
    Gen.choose(-100, 100)))

  val stubOrEmpty = Gen.oneOf("a", "")
  val stubs = stubOrEmpty map (Stub.apply)
  val parts = for {
    l <- stubOrEmpty
    r <- stubOrEmpty
    w <- Gen.choose(1, 10)
  } yield Part(l, w, r)

  nameProps("wcMonoid", monoidLaws(
    Chapter10.wcMonoid,
    parts))
}
