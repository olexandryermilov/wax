package wax.exercise.monadicparser


import cats.{Monad, Semigroup}
import cats.data.NonEmptyList

import scala.language.higherKinds

sealed trait Validated[+E, +A]
case class Valid[+A](a: A) extends Validated[Nothing, A]
case class Invalid[+E](e: E) extends Validated[E, Nothing]

//TODO Valik: think of an application for a monad here
object ConfigValidator {

  private def validateHost(field: String, h: String): Validated[NonEmptyList[String], String] =
    if (h.matches("[a-z0-9]+[\\.]{1}[a-z0-9]+[\\.]{1}[a-z0-9]+[\\.]{1}[a-z0-9]+")) Valid(h)
    else Invalid(NonEmptyList.one("host must be a proper hostname/ip without port"))


  implicit def validatedMonad[E: Semigroup]: Monad[Validated[E, ?]] = ???

  //TODO Validate host using monad
  def validateDbHost(cfg: HostConfig): Validated[NonEmptyList[String], String] = ???

}
