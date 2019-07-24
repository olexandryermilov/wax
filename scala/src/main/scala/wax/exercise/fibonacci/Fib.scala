package wax.exercise.fibonacci

import cats.Monoid
import wax.exercise.fibonacci.ExpUtils.exp
import wax.utils.Benchmark

import scala.math.BigInt._
import scala.math.BigInt

object Main extends App {
  println("Matrix")
  bench(Fib.fibOtEn)

  println()

  println("Tail recursive")
  bench(Fib.fibTailRec)

  def bench(f: BigInt => BigInt): Unit =
    Seq(10, 100, 1000, 10000, 100000, 1000000, 10000000).foreach { en =>
      val res = Benchmark(f(en))
      println(s"$en -> ${res.duration.toMillis} ms")
    }
}

object Fib {

  case class Matrix2x2(a11: BigInt, a12: BigInt, a21: BigInt, a22: BigInt)

  implicit val matrixMonoid: Monoid[Matrix2x2] = ???

  def fibOtEn(en: BigInt): BigInt = exp(
    Matrix2x2(
      1, 1,
      1, 0
    ),
    en
  ).a21

  private val Zero = BigInt(0)
  def fibTailRec(en: BigInt): BigInt = {
    def secretWeapon(n: BigInt, a: BigInt, b: BigInt): BigInt = n match {
      case Zero => a
      case _    => secretWeapon(n - 1, b, a + b)
    }

    secretWeapon(en, 0, 1)
  }
}


object ExpUtils {
  def exp[T: Monoid](n: T, power: BigInt): T =
    if (power < 0) throw new IllegalArgumentException("Something sad happened")
    else if (power == 0) Monoid.empty
    else if (power == 1) n
    //                power/2
    //n^power = (n^2)^
    else if (power % 2 == 0) ExpUtils.exp(Monoid.combine(n, n), power / 2)
    //                    (power-1)/2
    //n^power = n * (n^2)^
    else Monoid.combine(n, ExpUtils.exp(Monoid.combine(n, n), (power - 1) / 2))

  def expTailRec[T: Monoid](n: T, power: Int): T = {
    def exp(y: T, x: T, n: BigInt): T =
      if (power < 0) throw new IllegalArgumentException("Something sad happened")
      else if (power == 0) y
      else if (power == 1) Monoid.combine(x, y)
      else if (power % 2 == 0) exp(y, Monoid.combine(x, x), power / 2)
      else exp(Monoid.combine(x, y), Monoid.combine(x, x), (power - 1) / 2)

    exp(Monoid.empty[T], n, power)
  }
}
