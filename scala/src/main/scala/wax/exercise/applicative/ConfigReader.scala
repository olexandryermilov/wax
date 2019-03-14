package wax.exercise.applicative

import java.io.File

import cats.effect.IO
import cats.implicits._

import scala.io.Source

object ConfigReader {
  val validConfigFile = "validConfig"
  val invalidConfigFile = "typicalBorisConfig"

  private def readFile(fileName: String): IO[String] = IO {
    val file = new File(this.getClass.getClassLoader.getResource(s"applicative/$fileName").toURI)
    Source.fromFile(file).getLines.toString()
  }

  //TODO read 2 configs
  def readConfigs: IO[Configs] = ???

}

case class Configs(validConfig: String, typicalBorisConfig: String)
