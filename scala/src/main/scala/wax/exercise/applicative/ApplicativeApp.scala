package wax.exercise.applicative

import cats.data.NonEmptyList

object ApplicativeApp extends App {


  ConfigValidator.validateConfig(8080,"127.0.0.1", "root","sa","rkt",3300) ==
    Valid(Config(8080, "127.0.0.1", "root", "sa", "rkt", 3300))

  ConfigValidator.validateConfig(-42, "127.0.0.1:3300", "${username}", "is a happy", "pug", 3300) ==
    Invalid(NonEmptyList.of(
      ConfigError("appPort", "port must be an int between 0 and 65536"),
      ConfigError("dbHost", "host must be a proper hostname/ip without port")
    ))
}
