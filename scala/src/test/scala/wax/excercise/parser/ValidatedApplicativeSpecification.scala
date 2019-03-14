package wax.excercise.parser

import cats.{Applicative, Eq}
import cats.data.NonEmptyList
import cats.laws.discipline.ApplicativeTests
import cats.tests.CatsSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import wax.exercise.parser._

class ValidatedApplicativeSpecification extends CatsSuite {
  implicit val validatedApplicative: Applicative[Validated[NonEmptyList[Int], ?]] = ConfigValidator.validatedApplicative[NonEmptyList[Int]]

  implicit val eq: Eq[Validated[NonEmptyList[Int], Int]] = _ == _
  implicit val eq2: Eq[Validated[NonEmptyList[Int], (Int, Int, Int)]] = _ == _

  {
    implicit val arbitraryValid: Arbitrary[Validated[NonEmptyList[Int], Int]] = Arbitrary {
      arbitrary[Int] map Valid.apply
    }

    implicit val arbitraryValidF: Arbitrary[Validated[NonEmptyList[Int], Int => Int]] = Arbitrary {
        arbitrary[Int => Int] map Valid.apply
    }

    checkAll("Validated.Valid<*>Valid.ApplicativeLaws",
      ApplicativeTests[Validated[NonEmptyList[Int], ?]](validatedApplicative).applicative[Int, Int, Int]
    )
  }

  {
    implicit val arbitraryInvalid: Arbitrary[Validated[NonEmptyList[Int], Int]] = Arbitrary {
      arbitrary[Int] map (i => Invalid(NonEmptyList.one(i)))
    }

    implicit val arbitraryValidF: Arbitrary[Validated[NonEmptyList[Int], Int => Int]] = Arbitrary {
        arbitrary[Int => Int] map Valid.apply
    }

    checkAll("Validated.Invalid<*>Valid.ApplicativeLaws",
      ApplicativeTests[Validated[NonEmptyList[Int], ?]](validatedApplicative).applicative[Int, Int, Int]
    )
  }

  {
    implicit val arbitraryInvalid: Arbitrary[Validated[NonEmptyList[Int], Int]] = Arbitrary {
      arbitrary[Int] map (i => Invalid(NonEmptyList.one(i)))
    }

    implicit val arbitraryValidF: Arbitrary[Validated[NonEmptyList[Int], Int => Int]] = Arbitrary {
      arbitrary[Int] map (i => Invalid(NonEmptyList.one(i)))
    }

    checkAll("Validated.Invalid<*>Invalid.ApplicativeLaws",
      ApplicativeTests[Validated[NonEmptyList[Int], ?]](validatedApplicative).applicative[Int, Int, Int]
    )
  }
}
