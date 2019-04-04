package wax.exercise.monadicparser

object Application extends App {

  val dbConfigString = ConfigReader.readConfig.unsafeRunSync()

  val dbConfig = ConfigParser.parseConfig("test", dbConfigString)
  assert(ConfigValidator.validateDbHost(dbConfig) == Valid("db.prod.com"))
}
