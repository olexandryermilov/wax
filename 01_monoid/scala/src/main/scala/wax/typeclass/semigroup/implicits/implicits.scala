package wax.typeclass.semigroup

package object implicits {
  implicit val intSemigroup: Semigroup[Int] = new Semigroup[Int] {
    override def append(x: Int, y: Int): Int = ???
  }

  implicit val stringSemigroup: Semigroup[String] = new Semigroup[String] {
    override def append(x: String, y: String): String = ???
  }

  implicit def listSemigroup[A]: Semigroup[List[A]] = new Semigroup[List[A]] {
    override def append(x: List[A], y: List[A]): List[A] = ???
  }

  implicit class SemigroupOps[A: Semigroup](x: A) {
    def append(y: A): A = Semigroup[A].append(x, y)
  }
}
