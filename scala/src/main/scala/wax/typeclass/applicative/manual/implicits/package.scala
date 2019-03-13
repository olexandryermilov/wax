package wax.typeclass.applicative.manual

import wax.typeclass.applicative.manual.typeclass.Applicative

import scala.language.higherKinds

package object implicits {
  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] =
      ff match {
        case Some(f) => fa map f
        case None => None
      }
  }

  implicit val listApplicative: Applicative[List] = new Applicative[List] {
    override def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] =
      ff match {
        case h :: tail => (fa map h) ::: ap(tail)(fa)
        case Nil => Nil
      }
  }

  implicit def functionApplicative[A]: Applicative[Function[A, ?]] = new Applicative[Function[A, ?]] {
    override def ap[B, C](f: Function[A, B => C])(fa: A => B): A => C =
      (a: A) => f(a)(fa(a))
  }

  implicit class ApplicativeOps[A, F[_]: Applicative](fa: F[A]) {
    def ap[B](f: F[A => B]): F[B] = Applicative[F].ap(f)(fa)
  }
}
