package wax

import cats.Monoid
import wax.ExpUtils.exp
import wax.Matrices.Matrix2x2

object Fib extends App {
  val n = 0
  val matrix = Matrix2x2(1, 1,
                         1, 0)

  val result = matrix ^ n
  println(result.a21)
}

object Matrices {
  case class Matrix2x2(a11: Int, a12: Int, a21: Int, a22: Int)

  implicit class MatrixOps(val matrix: Matrix2x2) extends AnyVal {
    def ^(power: Int): Matrix2x2 = exp(matrix, power)
  }

  implicit val matrixMonoid: Monoid[Matrix2x2] = ???
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