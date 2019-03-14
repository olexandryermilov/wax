package wax.exercise.applicative

import cats.data.NonEmptyList

object ApplicativeApp extends App {

  val Configs(validConfigStr, invalidConfigStr) = ConfigReader.readConfigs.unsafeRunSync()

  val validConfig = ConfigParser.parseConfig(validConfigStr)
  val invalidConfig = ConfigParser.parseConfig(invalidConfigStr)

  ConfigValidator.validateConfig(validConfig) ==
    Valid(Config(8080, "127.0.0.1", "root", "sa", "rkt", 3300))

  ConfigValidator.validateConfig(invalidConfig) ==
    Invalid(NonEmptyList.of(
      ConfigValidationError("appPort", "port must be an int between 0 and 65536"),
      ConfigValidationError("dbHost", "host must be a proper hostname/ip without port")
    ))
}
