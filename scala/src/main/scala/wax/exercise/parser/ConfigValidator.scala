package wax.exercise.parser

import cats.Applicative
import cats.data.NonEmptyList
import cats.kernel.Semigroup
import scala.language.postfixOps
import cats.implicits._

sealed trait Validated[+E, +A]
case class Valid[+A](a: A) extends Validated[Nothing, A]
case class Invalid[+E](e: E) extends Validated[E, Nothing]

case class ConfigValidationError(field: String, message: String)

object ConfigValidator {

  private def validatePort(field: String, p: Int): Validated[NonEmptyList[ConfigValidationError], Int] =
    if (p >= 0 && p <= 65536) Valid(p)
    else Invalid(NonEmptyList.one(ConfigValidationError(field, "port must be an int between 0 and 65536")))

  private def validateHost(field: String, h: String): Validated[NonEmptyList[ConfigValidationError], String] =
    if (h.matches("[a-z0-9]+[\\.]{1}[a-z0-9]+[\\.]{1}[a-z0-9]+[\\.]{1}[a-z0-9]+")) Valid(h)
    else Invalid(NonEmptyList.one(ConfigValidationError(field, "host must be a proper hostname/ip without port")))


  implicit def validatedApplicative[E: Semigroup]: Applicative[Validated[E, ?]] =
    new Applicative[Validated[E, ?]] {
      override def pure[A](x: A): Validated[E, A] = Valid(x)

      override def ap[A, B](ff: Validated[E, A => B])
                           (fa: Validated[E, A]): Validated[E, B] = {
        ff match {
          case Valid(atob) => fa match {
            case Valid(a) => Valid(atob(a))
            case e@Invalid(_) => e
          }
          case e@Invalid(error1) => fa match {
            case Valid(_) => e
            case Invalid(error2) => Invalid(error1 |+| error2)
          }
        }
      }
    }

  //TODO Validate config using applicative
  def validateConfig(config: Config): Validated[NonEmptyList[ConfigValidationError], Config] = ???
}
