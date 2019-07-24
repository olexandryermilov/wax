package wax.typeclass.semigroup.laws.manual

import org.scalacheck.Prop.{AnyOperators, forAll}
import org.scalacheck.{Arbitrary, Properties}
import wax.typeclass.semigroup.manual.implicits._
import wax.typeclass.semigroup.manual.typeclass._

import scala.reflect.ClassTag

object SemigroupSpec extends Properties("Semigroup") with SemigroupSpec {
  include(semigroup[Float])
  include(semigroup[List[Int]])
}

trait SemigroupSpec {
  def semigroup[A](implicit sg: Semigroup[A], ar: Arbitrary[A], tag: ClassTag[A]): Properties =
    new Properties(s"Semigroup[${tag.toString}]") {
      property("associativity") = forAll { (a: A, b: A, c: A) =>
        sg.combine(sg.combine(a, b), c) =? sg.combine(a, sg.combine(b, c))
      }
    }
}
