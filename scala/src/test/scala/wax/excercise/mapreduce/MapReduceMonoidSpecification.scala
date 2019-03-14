package wax.excercise.mapreduce

import cats.kernel.laws.discipline.MonoidTests
import cats.tests.CatsSuite
import wax.exercise.mapreduce.MapReduceRunner

class MapReduceMonoidSpecification extends CatsSuite {
  implicit val monoid = MapReduceRunner.monoid

//  implicit val eq: Eq[???] = ???
//  implicit val arbitraryM: Arbitrary[???] = ???

//  checkAll("MapReduce.MonoidLaws", MonoidTests[???](monoid).monoid)

}
