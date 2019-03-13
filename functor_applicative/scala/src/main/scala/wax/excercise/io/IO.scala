package wax.excercise.io

import java.io.File

import scala.io.Source
import cats.effect.IO

object IOApp extends App {

  def readConfig(path: String): IO[String] = IO {
    val file = new File(this.getClass.getClassLoader.getResource(path).toURI)
    Source.fromFile(file).getLines.toString()
  }


}
