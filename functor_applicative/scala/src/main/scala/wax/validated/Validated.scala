package wax.validated

import cats.data.NonEmptyList

object Validated extends App {
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
    dbUsername = "yarik",
    dbPw       = "is a happy",
    dbSchema   = "pug",
    dbPort     = 3300,
  )

  def validateConfig(config: Config): Validated[ConfigError, Config] = ???

  validateConfig(validConfig) == Valid(validConfig)
  validateConfig(typicalBorisConfig) == Invalid(NonEmptyList(
    ConfigError("appPort", "port must be a positive value between 0 and 65536"),
    ConfigError("dbHost", "db host must be a proper hostname/ip without port") ::
    Nil
  ))
}

object Validator {
  def validatePort(p: Int) =
    if (p >= 0 && p <= 65536) Valid(p)
    else Invalid(ConfigError("appPort", "port must be a positive value between 0 and 65536"))
  def validateHost(h: String) = ???
}

case class Config(appPort: Int, dbHost: String, dbUsername: String, dbPw: String, dbSchema: String, dbPort: Int)

case class ConfigError(field: String, message: String)

sealed trait Validated[+E, +A]
case class Valid[+A](a: A) extends Validated[Nothing, A]
case class Invalid[+E](e: E) extends Validated[E, Nothing]