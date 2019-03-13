package wax.typeclass.semigroup

package object catz {
  implicit val intSemigroup: cats.Semigroup[Int] = new cats.Semigroup[Int] {
    override def combine(x: Int, y: Int): Int = ???
  }

  implicit val stringSemigroup: cats.Semigroup[String] = new cats.Semigroup[String] {
    override def combine(x: String, y: String): String = ???
  }

  implicit def listSemigroup[A]: cats.Semigroup[List[A]] = new cats.Semigroup[List[A]] {
    override def combine(x: List[A], y: List[A]): List[A] = ???
  }
}
