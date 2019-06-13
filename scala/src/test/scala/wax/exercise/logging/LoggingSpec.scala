package wax.exercise.logging

import cats.{Eq, Monoid}
import cats.effect.IO
import cats.kernel.laws.discipline.MonoidTests
import cats.tests.CatsSuite
import org.scalacheck.Arbitrary

class LoggingSpec extends CatsSuite {
  def ioMonoid[A: Monoid]: Monoid[IO[A]] = ???

  def ioEq[A: Eq]: Eq[IO[A]] = (x: IO[A], y: IO[A]) => Eq[A].eqv(x.unsafeRunSync(), y.unsafeRunSync())

  def ioArb[A: Arbitrary]: Arbitrary[IO[A]] = Arbitrary {
    for {
      v <- Arbitrary.arbitrary[A]
    } yield IO.pure(v)
  }

  checkAll("IO[Int].MonoidLaws", MonoidTests[IO[Int]](ioMonoid).monoid(ioArb, ioEq))
}
