package wax.exercise.monadicparser

import java.io.File

import cats.effect.IO

import scala.io.Source
import cats.implicits._

import scala.language.higherKinds

object ConfigReader {
  private def readFile(fileName: String): IO[String] = IO {
    val file = new File(this.getClass.getClassLoader.getResource(s"monadicparser/$fileName").toURI)
    Source.fromFile(file).getLines.toString()
  }

  def readConfig: IO[String] = ???
}
