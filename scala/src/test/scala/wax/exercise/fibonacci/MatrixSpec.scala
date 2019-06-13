package wax.exercise.fibonacci

import cats.{Eq, Monoid}
import cats.kernel.laws.discipline.MonoidTests
import cats.tests.CatsSuite
import org.scalacheck.{Arbitrary, Gen}
import wax.exercise.fibonacci.Fib
import wax.exercise.fibonacci.Fib.Matrix2x2

class MatrixSpec extends CatsSuite {
  val matrixMonoid: Monoid[Matrix2x2] = Fib.matrixMonoid

  implicit val eq: Eq[Matrix2x2] = (x: Matrix2x2, y: Matrix2x2) => x == y
  implicit val arbM: Arbitrary[Matrix2x2] = Arbitrary {
    for {
      a11 <- Gen.choose(2, 1000)
      a12 <- Gen.choose(2, 1000)
      a21 <- Gen.choose(2, 1000)
      a22 <- Gen.choose(2, 1000)
    } yield Matrix2x2(a11, a12, a21, a22)
  }

  checkAll("Matrix2x2.MonoidLaws", MonoidTests[Matrix2x2](matrixMonoid).monoid)

}
