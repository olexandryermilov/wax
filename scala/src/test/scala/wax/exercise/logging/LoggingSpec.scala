package wax.exercise.logging

import cats.Eq
import cats.kernel.laws.discipline.MonoidTests
import cats.tests.CatsSuite
import wax.exercise.logging.Logging._

class LoggingSpec extends CatsSuite {

  checkAll("Unit.MonoidLaws", MonoidTests[Unit](monoidUnit).monoid)

  // I don't know how to test function properties in Scala
  // See http://www.cse.chalmers.se/~rjmh/QuickCheck/manual_body.html#19
  implicit def stringFunctionEq[A: Eq]: Eq[Function[String, A]] = new Eq[Function[String, A]] {
    override def eqv(x: Function[String, A], y: Function[String, A]): Boolean = x("some-string") == y("some-string")
  }

  checkAll("Function.MonoidLaws", MonoidTests[Function[String, String]](monoidFunction[String, String]).monoid)
}
