package wax.semigroup.laws.cats

import cats.kernel.laws.discipline.SemigroupTests
import cats.tests.CatsSuite
import wax.typeclass.semigroup.catz._

class SemigroupSpecification extends CatsSuite {
  // pass implicit here (stringSemigroup) to make sure that our implementation is used
  checkAll("Int.SemigroupLaws", SemigroupTests[Int](intSemigroup).semigroup)

  // pass implicit here (stringSemigroup) to make sure that our implementation is used
  checkAll("String.SemigroupLaws", SemigroupTests[String](stringSemigroup).semigroup)

  // pass implicit here (listSemigroup) to make sure that our implementation is used
  checkAll("List[Int].SemigroupLaws", SemigroupTests[List[Int]](listSemigroup).semigroup)
}
