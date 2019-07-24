package wax.typeclass.semigroup.manual

/*

Task:

1. Create Semigroup typeclass.
2. Fill missing instances in manual.instances.
3. Make manual.SemigroupSpec tests green.

 */

package object typeclass {

  //
  // Laws:
  //   1. ⊕ is internal operation: ∀ a ∈ A, b ∈ A: a ⊕ b ∈ A
  //   2. (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)
  //
  trait Semigroup[A] {
    ???
  }

  object Semigroup {
    def apply[A: Semigroup]: Semigroup[A] = implicitly[Semigroup[A]]
  }

}
