package wax.typeclass.semigroup

package object catz {
  implicit val stringSemigroup: cats.Semigroup[String] = new cats.Semigroup[String] {
    override def combine(x: String, y: String): String = x + y // y + x is still a valid semigroup
  }

  implicit def listSemigroup[A]: cats.Semigroup[List[A]] = new cats.Semigroup[List[A]] {
    override def combine(x: List[A], y: List[A]): List[A] = x ++ y // y ++ x is still a valid semigroup
  }
}
