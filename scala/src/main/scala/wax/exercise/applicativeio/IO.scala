package wax.exercise.applicativeio

import java.io.File

import cats.effect.IO

import scala.io.Source

object IOApp extends App {

  def readConfig(path: String): IO[String] = IO {
    val file = new File(this.getClass.getClassLoader.getResource(path).toURI)
    Source.fromFile(file).getLines.toString()
  }


}
