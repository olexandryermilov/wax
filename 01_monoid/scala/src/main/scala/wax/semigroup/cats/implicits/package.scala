package wax.semigroup.cats

import cats.Semigroup

package object implicits {
  implicit val stringSemigroup: Semigroup[String] = new Semigroup[String] {
    override def combine(x: String, y: String): String = x + y // y + x is still a valid semigroup
  }

  implicit def listSemigroup[A]: Semigroup[List[A]] = new Semigroup[List[A]] {
    override def combine(x: List[A], y: List[A]): List[A] = x ++ y // y ++ x is still a valid semigroup
  }
}
