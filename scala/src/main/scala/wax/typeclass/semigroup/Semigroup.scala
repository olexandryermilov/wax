package wax.typeclass.semigroup

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
