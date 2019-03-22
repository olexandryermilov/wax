package wax.exercise.monadicparser

object Application extends App {

  val dbConfig = ??? //ConfigReader.readConfig.unsafeRunSync()

  val dbHost = ??? //ConfigParser.parseConfig(validConfigStr)

//  assert(ConfigValidator.validateConfig(validConfig) ==
//    Valid(Config(8080, "127.0.0.1", "root", "sa", "rkt", 3300)))
}
