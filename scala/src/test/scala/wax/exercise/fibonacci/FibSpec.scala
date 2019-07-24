package wax.exercise.fibonacci

import cats.tests.CatsSuite
import org.scalacheck.{Gen, Prop}

class FibSpec extends CatsSuite {

  fibIsFibProp("tailRecFib", Fib.fibTailRec)
  fibIsFibProp("matrixFib", Fib.fibOtEn)

  val enGen: Gen[BigInt] = Gen.choose(2, 1000).map(a => BigInt(a))
  def fibIsFibProp(name: String, f: BigInt => BigInt): Unit =
    test(s"$name is fib") {
      Prop.forAll(enGen) { en: BigInt =>
        val a = f(en - 2)
        val b = f(en - 1)
        val c = f(en)
        a + b == c
      }.check
    }

}
