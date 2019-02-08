package wax.validated

import cats.data.NonEmptyList

object Validated extends App {
  val validConfig: Map[String, String] = Map(
    "appPort"    -> "8080",
    "dbHost"     -> "127.0.0.1",
    "dbUsername" -> "yarik",
    "dbPw"       -> "is a happy",
    "dbSchema"   -> "pug",
    "dbPort"     -> "3300")

  val invalidConfig: Map[String, String] = Map(
    "appPort"    -> "808o",
    "dbHost"     -> "127.0.0.1",
    "dbPw"       -> "is a happy",
    "dbSchema"   -> "pug",
    "dbPort"     -> "java.lang.NullPointerException")

  def validateConfig(config: Map[String, String]): Validated[ConfigError, Map[String, String]] = ???

  validateConfig(validConfig) == Valid(validConfig)
  validateConfig(invalidConfig) == Invalid(
    NonEmptyList(ParseError("appPort"),
    MissingConfig("dbUsername") ::
    ParseError("dbPort") ::
    Nil))
}

sealed abstract class ConfigError
case class MissingConfig(field: String) extends ConfigError
case class ParseError(field: String) extends ConfigError

sealed trait Validated[+E, +A]
case class Valid[+A](a: A) extends Validated[Nothing, A]
case class Invalid[+E](e: E) extends Validated[E, Nothing]