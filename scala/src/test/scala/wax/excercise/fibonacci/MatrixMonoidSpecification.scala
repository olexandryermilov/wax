package wax.excercise.fibonacci

import cats.{Eq, Monoid}
import cats.kernel.laws.discipline.MonoidTests
import cats.tests.CatsSuite
import org.scalacheck.{Arbitrary, Gen}
import wax.exercise.fibonacci.Fib
import wax.exercise.fibonacci.Fib.Matrix2x2

class MatrixMonoidSpecification extends CatsSuite {
  implicit val matrixMonoid: Monoid[Matrix2x2] = Fib.matrixMonoid

  implicit val eq: Eq[Matrix2x2] = (x: Matrix2x2, y: Matrix2x2) => x == y
  implicit val arbM: Arbitrary[Matrix2x2] = Arbitrary {
    for {
      a11 <- Arbitrary.arbitrary[Int]
      a12 <- Arbitrary.arbitrary[Int]
      a21 <- Arbitrary.arbitrary[Int]
      a22 <- Arbitrary.arbitrary[Int]
    } yield Matrix2x2(a11, a12, a21, a22)
  }

  checkAll("Matrix2x2.MonoidLaws", MonoidTests[Matrix2x2](matrixMonoid).monoid)

}
