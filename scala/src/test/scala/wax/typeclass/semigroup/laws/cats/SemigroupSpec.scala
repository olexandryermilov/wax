package wax.typeclass.semigroup.laws.cats

import cats.kernel.laws.discipline.SemigroupTests
import cats.tests.CatsSuite
import wax.typeclass.semigroup.cats.implicits._

class SemigroupSpec extends CatsSuite {
  // pass implicit here (stringSemigroup) to make sure that our implementation is used
  checkAll("Int.SemigroupLaws", SemigroupTests[Int](intSemigroup).semigroup)

  // pass implicit here (stringSemigroup) to make sure that our implementation is used
  checkAll("String.SemigroupLaws", SemigroupTests[String](stringSemigroup).semigroup)
}
