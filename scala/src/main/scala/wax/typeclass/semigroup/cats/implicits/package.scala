package wax.typeclass.semigroup.cats

/*

Task:

1. Fill missing implementation.
2. Make cats.SemigroupSpec tests green.

 */

package object implicits {
  implicit val intSemigroup: cats.Semigroup[Int] = new cats.Semigroup[Int] {
    override def combine(x: Int, y: Int): Int = ???
  }

  implicit val stringSemigroup: cats.Semigroup[String] = new cats.Semigroup[String] {
    override def combine(x: String, y: String): String = ???
  }
}
