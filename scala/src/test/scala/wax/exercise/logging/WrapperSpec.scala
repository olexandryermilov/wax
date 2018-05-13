package wax.exercise.logging

import cats.Eq
import cats.kernel.laws.discipline.MonoidTests
import cats.tests.CatsSuite
import org.scalacheck.Arbitrary
import wax.exercise.logging.Wrapper._

class WrapperSpec extends CatsSuite {
  implicit def wrapperEq[A: Eq]: Eq[Wrapper[A]] = (x: Wrapper[A], y: Wrapper[A]) => Eq[A].eqv(x.value, y.value)

  implicit def arbWrapper[A: Arbitrary]: Arbitrary[Wrapper[A]] = Arbitrary {
    for {
      v <- Arbitrary.arbitrary[A]
    } yield Wrapper(v)
  }

  checkAll("Wrapper[Int].MonoidLaws", MonoidTests[Wrapper[Int]].monoid)
}
