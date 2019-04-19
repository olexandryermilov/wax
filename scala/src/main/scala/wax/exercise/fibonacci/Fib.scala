package wax.exercise.fibonacci

import cats.Monoid
import wax.exercise.fibonacci.ExpUtils.exp

object Fib extends App {

  case class Matrix2x2(a11: Int, a12: Int, a21: Int, a22: Int)

  implicit val matrixMonoid: Monoid[Matrix2x2] = ???

  def fibOtEn(en: Int) = exp(
    Matrix2x2(1, 1,
              1, 0),
    en
  ).a21

  println(fibOtEn(0))
}



object ExpUtils {
  def exp[T: Monoid](n: T, power: Int): T =
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
    def exp(y: T, x: T, n: Int): T =
      if (power < 0) throw new IllegalArgumentException("Something sad happened")
      else if (power == 0) y
      else if (power == 1) Monoid.combine(x, y)
      else if (power % 2 == 0) exp(y, Monoid.combine(x, x), power / 2)
      else exp(Monoid.combine(x, y), Monoid.combine(x, x), (power - 1) / 2)

    exp(Monoid.empty[T], n, power)
  }
}
