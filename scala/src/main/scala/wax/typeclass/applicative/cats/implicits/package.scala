package wax.typeclass.applicative.cats

import cats.Applicative

package object implicits {
  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def pure[A](a: A): Option[A] = Some(a)

    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = {
      ff match {
        case Some(f) => fa map f
        case None => None
      }
    }
  }

  implicit val listApplicative: Applicative[List] = new Applicative[List] {
    override def pure[A](a: A): List[A] = List(a)

    override def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] = {
      ff match {
        case f :: tail => fa.map(f) ::: ap(tail)(fa)
        case Nil => Nil
      }
    }
  }

  implicit def functionApplicative[T]: Applicative[Function[T, ?]] = new Applicative[Function[T, ?]] {
    override def pure[A](a: A): Function[T, A] = _ => a

    override def ap[A, B](ff: Function[T, A => B])
                         (fa: Function[T, A]): Function[T, B] = (t: T) => ff(t)(fa(t))
  }
}
