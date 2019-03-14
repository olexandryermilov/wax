package wax.excercise.mapreduce

import cats.Monoid
import cats.kernel.laws.discipline.MonoidTests
import cats.tests.CatsSuite
import wax.exercise.mapreduce.MapReduceRunner

class MapReduceMonoidSpecification extends CatsSuite {
  implicit val monoid = MapReduceRunner.monoid

  checkAll("MapReduce.MonoidLaws", MonoidTests[Map[String, Int]](monoid).monoid)

}
