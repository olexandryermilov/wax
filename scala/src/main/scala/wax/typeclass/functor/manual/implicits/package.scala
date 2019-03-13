package wax.typeclass.functor.manual

import wax.typeclass.functor.manual.typeclass.Functor

import scala.language.higherKinds

package object implicits {
  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def fmap[A, B](f: A => B)(fa: Option[A]): Option[B] =
      fa match {
        case Some(a) => Some(f(a))
        case None => None
      }
  }

  implicit val listFunctor: Functor[List] = new Functor[List] {
    override def fmap[A, B](f: A => B)(fa: List[A]): List[B] =
      fa match {
        case h :: tail => f(h) :: fmap(f)(tail)
        case Nil => Nil
      }
  }

  implicit def functionFunctor[A]: Functor[Function[A, ?]] = new Functor[Function[A, ?]] {
    override def fmap[B, C](f: B => C)(fa: A => B): A => C = fa.andThen(f)
  }

  implicit class FunctorOps[A, F[_]: Functor](fa: F[A]) {
    def fmap[B](f: A => B): F[B] = Functor[F].fmap(f)(fa)
  }
}
