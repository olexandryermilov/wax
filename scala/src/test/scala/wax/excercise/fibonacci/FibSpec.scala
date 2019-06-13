package wax.excercise.fibonacci

import cats.tests.CatsSuite
import org.scalacheck.{Gen, Prop}
import wax.exercise.fibonacci.Fib

class FibSpec extends CatsSuite {

  fibIsFibProp(Fib.fibTailRec)
  fibIsFibProp(Fib.fibOtEn)

  val enGen: Gen[BigInt] = Gen.choose(2, 1000).map(a => BigInt(a))
  def fibIsFibProp(f: BigInt => BigInt): Unit =
    test("fib is fib") {
      Prop.forAll(enGen) { en: BigInt =>
        val a = f(en - 2)
        val b = f(en - 1)
        val c = f(en)
        a + b == c
      }.check
    }

}
