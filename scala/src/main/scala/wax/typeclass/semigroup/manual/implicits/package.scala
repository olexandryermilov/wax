package wax.typeclass.semigroup.manual

import wax.typeclass.semigroup.manual.typeclass.Semigroup

package object implicits {
  implicit val floatSemigroup: Semigroup[Float] = new Semigroup[Float] {
    override def combine(x: Float, y: Float): Float = ???
  }

  implicit def listSemigroup[A]: Semigroup[List[A]] = new Semigroup[List[A]] {
    override def combine(x: List[A], y: List[A]): List[A] = ???
  }

  implicit class SemigroupOps[A: Semigroup](x: A) {
    def combine(y: A): A = Semigroup[A].combine(x, y)
  }
}
