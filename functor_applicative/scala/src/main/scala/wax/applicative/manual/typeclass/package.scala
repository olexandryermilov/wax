package wax.applicative.manual

import scala.language.higherKinds

package object typeclass {

  //
  // Laws:
  // TODO
  //
  trait Applicative[F[_]] {
    def ap[A, B](f: F[A => B])(fa: F[A]): F[B]
  }

  object Applicative {
    def apply[F[_]: Applicative]: Applicative[F] = implicitly[Applicative[F]]
  }

}
