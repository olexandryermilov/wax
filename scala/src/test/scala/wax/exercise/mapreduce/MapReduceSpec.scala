package wax.exercise.mapreduce

import cats.Monoid
import cats.kernel.laws.discipline.MonoidTests
import cats.tests.CatsSuite
import wax.exercise.mapreduce.MapReduce
import wax.exercise.mapreduce.MapReduce.Result

class MapReduceSpec extends CatsSuite {
  val monoid: Monoid[Result[Int]] = MapReduce.monoid

  checkAll("MapReduce.MonoidLaws", MonoidTests[Map[String, Int]](monoid).monoid)

}
