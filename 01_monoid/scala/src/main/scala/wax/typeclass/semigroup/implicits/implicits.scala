package wax.typeclass.semigroup

package object implicits {
  implicit val stringSemigroup: Semigroup[String] = new Semigroup[String] {
    override def append(x: String, y: String): String = x + y
  }

  implicit def listSemigroup[A]: Semigroup[List[A]] = new Semigroup[List[A]] {
    override def append(x: List[A], y: List[A]): List[A] = x ++ y
  }

  implicit class SemigroupOps[A: Semigroup](x: A) {
    def append(y: A): A = Semigroup[A].append(x, y)
  }
}
