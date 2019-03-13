package wax.typeclass.applicative.manual

import scala.language.higherKinds

package object typeclass {

  //
  // Laws:
  // 1. Identity:
  //    (fa: F[A]) =>
  //      F.pure((a: A) => a).ap(fa) <-> fa
  // 2. Homomorphism:
  //    (a: A, f: A => B) =>
  //      F.pure(f).ap(F.pure(a)) <-> F.pure(f(a))
  // 3. Interchange:
  //    (a: A, ff: F[A => B]) =>
  //      ff.ap(F.pure(a)) <-> F.pure((f: A => B) => f(a)).ap(ff)
  // 4. Map:
  //    (fa: F[A], f: A => B) =>
  //      fa.map(f) <-> F.pure(f).ap(fa)
  // 5. Composition:
  //    (fa: F[A], fab: F[A => B], fbc: F[B => C]) => {
  //      val compose: (B => C) => (A => B) => (A => C) = _.compose
  //      F.pure(compose).ap(fbc).ap(fab).ap(fa) <-> fbc.ap(fab.ap(fa))
  //    }
  //
  trait Applicative[F[_]] {
    def ap[A, B](f: F[A => B])(fa: F[A]): F[B]
  }

  object Applicative {
    def apply[F[_]: Applicative]: Applicative[F] = implicitly[Applicative[F]]
  }

}
