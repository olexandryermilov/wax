package wax.exercise.applicative

import cats.Applicative
import cats.data.NonEmptyList
import cats.kernel.Semigroup
import scala.language.postfixOps
import cats.implicits._

sealed trait Validated[+E, +A]
case class Valid[+A](a: A) extends Validated[Nothing, A]
case class Invalid[+E](e: E) extends Validated[E, Nothing]

case class Config(appPort: Int, dbHost: String, dbUsername: String, dbPw: String, dbSchema: String, dbPort: Int)
case class ConfigError(field: String, message: String)

object ConfigValidator {
  implicit def validatedApplicative[E: Semigroup]: Applicative[Validated[E, ?]] = ???

  private val hostnameRegex = "[a-z0-9]+[\\.]{1}[a-z0-9]+[\\.]{1}[a-z0-9]+[\\.]{1}[a-z0-9]+".r

  private def validatePort(field: String, p: Int): Validated[NonEmptyList[ConfigError], Int] =
    if (p >= 0 && p <= 65536) Valid(p)
    else Invalid(NonEmptyList.one(ConfigError(field, "port must be an int between 0 and 65536")))

  private def validateHost(field: String, h: String): Validated[NonEmptyList[ConfigError], String] =
    hostnameRegex.findFirstIn(h) match {
      case Some(s) => Valid(s)
      case None => Invalid(NonEmptyList.one(ConfigError(field, "host must be a proper hostname/ip without port")))
    }

  def validateConfig(appPort: Int,
                     dbHost: String,
                     dbUsername: String,
                     dbPw: String,
                     dbSchema: String,
                     dbPort: Int): Validated[NonEmptyList[ConfigError], Config] = ???

}
