package wax.validated

import cats.Applicative
import cats.data.NonEmptyList
import cats.kernel.Semigroup
import scala.language.postfixOps
import cats.implicits._

sealed trait Validated[+E, +A]
case class Valid[+A](a: A) extends Validated[Nothing, A]
case class Invalid[+E](e: E) extends Validated[E, Nothing]

object Validated extends App {
  implicit def validatedApplicative[E: Semigroup]: Applicative[Validated[E, ?]] = ???

  def validateConfig(config: Config): Validated[NonEmptyList[ConfigError], Config] = ???

  validateConfig(Configs.validConfig) == Valid(Configs.validConfig)
  validateConfig(Configs.typicalBorisConfig) == Invalid(NonEmptyList.of(
    ConfigError("appPort", "port must be an int between 0 and 65536"),
    ConfigError("dbHost", "host must be a proper hostname/ip without port")
  ))
}

object Validator {
  private val hostnameRegex = "[a-z0-9]+[\\.]{1}[a-z0-9]+[\\.]{1}[a-z0-9]+[\\.]{1}[a-z0-9]+".r

  def validatePort(field: String, p: Int): Validated[NonEmptyList[ConfigError], Int] =
    if (p >= 0 && p <= 65536) Valid(p)
    else Invalid(NonEmptyList.one(ConfigError(field, "port must be an int between 0 and 65536")))

  def validateHost(field: String, h: String): Validated[NonEmptyList[ConfigError], String] =
    hostnameRegex.findFirstIn(h) match {
      case Some(s) => Valid(s)
      case None => Invalid(NonEmptyList.one(ConfigError(field, "host must be a proper hostname/ip without port")))
    }
}

case class Config(appPort: Int, dbHost: String, dbUsername: String, dbPw: String, dbSchema: String, dbPort: Int)
case class ConfigError(field: String, message: String)

object Configs {
  val validConfig = Config(
    appPort    = 8080,
    dbHost     = "127.0.0.1",
    dbUsername = "root",
    dbPw       = "sa",
    dbSchema   = "rkt",
    dbPort     = 3300,
  )

  val typicalBorisConfig = Config(
    appPort    = -42,
    dbHost     = "127.0.0.1:3300",
    dbUsername = "username",
    dbPw       = "is a happy",
    dbSchema   = "pug",
    dbPort     = 3300,
  )
}
