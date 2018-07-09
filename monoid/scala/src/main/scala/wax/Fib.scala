package wax

import cats.Monoid
import wax.ExpUtils.exponentiate
import wax.Matrices.{Matrix, _}

class Fib {
  def fib(n: Int): Long = {
    val matrix: Matrix = Array(Array(1, 1),
                               Array(1, 0))

    val resMatrix = matrix ^ n
    resMatrix.get(1, 1)
  }
}

object Matrices {
  type Matrix = Array[Array[Int]]

  implicit class MatrixOps(val matrix: Matrix) extends AnyVal {
    def ^(power: Int): Matrix = exponentiate(matrix, power)(???)
    def get(n: Int, m: Int): Int = matrix(n)(m)
  }

}

object ExpUtils {
  def exponentiate[T: Monoid](n: T, power: Int): T =
    if (power < 0) throw new IllegalArgumentException("Something sad happened")
    else if (power == 0) Monoid.empty
    else if (power == 1) n
    //                power/2
    //n^power = (n^2)^
    else if (power % 2 == 0) ExpUtils.exponentiate(Monoid.combine(n, n), power / 2)(???)
    //                    (power-1)/2
    //n^power = n * (n^2)^
    else Monoid.combine(n, ExpUtils.exponentiate(Monoid.combine(n, n), (power - 1) / 2))(???)

  def exponentiateTailRec[T: Monoid](n: T, power: Int): T = {
    def exp(y: T, x: T, n: Int): T =
      if (power < 0) throw new IllegalArgumentException("Something sad happened")
      else if (power == 0) y
      else if (power == 1) Monoid.combine(x, y)
      else if (power % 2 == 0) exp(y, Monoid.combine(x, x), power / 2)
      else exp(Monoid.combine(x, y), Monoid.combine(x, x), (power - 1) / 2)

    exp(Monoid.empty[T], n, power)
  }
}