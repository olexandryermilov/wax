package wax.semigroup.laws.manual

import org.scalacheck.Prop.{AnyOperators, forAll}
import org.scalacheck.{Arbitrary, Properties}
import wax.typeclass.semigroup.Semigroup
import wax.typeclass.semigroup.implicits._

import scala.reflect.ClassTag

object SemigroupSpecification extends Properties("Semigroup") with SemigroupSpecificationSupport {
  include(semigroup[Int])
  include(semigroup[String])
  include(semigroup[List[Int]])
}

trait SemigroupSpecificationSupport {
  def semigroup[A](implicit sg: Semigroup[A], ar: Arbitrary[A], tag: ClassTag[A]): Properties =
    new Properties(s"Semigroup[${tag.toString}]") {
      property("associativity") = forAll { (a: A, b: A, c: A) =>
        sg.append(sg.append(a, b), c) =? sg.append(a, sg.append(b, c))
      }
    }
}
