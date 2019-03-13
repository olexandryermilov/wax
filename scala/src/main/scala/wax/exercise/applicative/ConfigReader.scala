package wax.exercise.applicative

import java.io.File

import cats.Applicative
import cats.effect.IO
import cats.implicits._
import scala.io.Source

object ConfigReader {

  private def readConfig(fileName: String): IO[String] = IO {
    val file = new File(this.getClass.getClassLoader.getResource(s"applicative_io/$fileName").toURI)
    Source.fromFile(file).getLines.toString()
  }

//  implicit val ioApplicative: Applicative[IO]
//
//  def readConfigs: IO[Configs] = {
//    val validConfigFile = "validConfig"
//    val typicalBorisConfigFile = "typicalBorisConfig"
//
//    readConfig(validConfigFile).fm
//  }

}

case class Configs(validConfig: String, typicalBorisConfig: String)
