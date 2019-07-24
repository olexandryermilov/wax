package wax.typeclass.semigroup.manual

/*

Task:

1. Fill missing instances in manual.instances.
2. Make manual.SemigroupSpec tests green.

 */

package object typeclass {

  //
  // Laws:
  //   1. ⊕ is internal operation: ∀ a ∈ A, b ∈ A: a ⊕ b ∈ A
  //   2. (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)
  //
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  object Semigroup {
    def apply[A: Semigroup]: Semigroup[A] = implicitly[Semigroup[A]]
  }

}
