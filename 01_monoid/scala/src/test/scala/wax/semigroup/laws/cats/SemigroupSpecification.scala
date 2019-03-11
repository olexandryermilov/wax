package wax.semigroup.laws.cats

import wax.semigroup.cats.implicits._
import cats.kernel.laws.discipline.SemigroupTests
import cats.tests.CatsSuite

class SemigroupSpecification extends CatsSuite {
  // pass implicit here (stringSemigroup) to make sure that our implementation is used
  checkAll("String.SemigroupLaws", SemigroupTests[String](stringSemigroup).semigroup)

  // pass implicit here (listSemigroup) to make sure that our implementation is used
  checkAll("List[Int].SemigroupLaws", SemigroupTests[List[Int]](listSemigroup).semigroup)
}
