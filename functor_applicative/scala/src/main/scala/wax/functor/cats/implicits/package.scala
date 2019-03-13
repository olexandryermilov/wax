package wax.functor.cats

import cats.Functor

package object implicits {
  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa match {
        case Some(a) => Some(f(a))
        case None => None
      }
  }

  implicit val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] =
      fa match {
        case h :: tail => f(h) :: map(tail)(f)
        case Nil => Nil
      }
  }

  implicit def functionFunctor[A]: Functor[Function[A, ?]] = new Functor[Function[A, ?]] {
    override def map[B, C](fa: A => B)(f: B => C): A => C = fa.andThen(f)
  }
}
