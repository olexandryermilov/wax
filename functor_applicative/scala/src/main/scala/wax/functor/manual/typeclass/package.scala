package wax.functor.manual

import scala.language.higherKinds

package object typeclass {

  //
  // Laws:
  // TODO
  //
  trait Functor[F[_]] {
    def fmap[A, B](f: A => B)(fa: F[A]): F[B]
  }

  object Functor {
    def apply[F: Functor]: Functor[F] = implicitly[Functor[F]]
  }

}
