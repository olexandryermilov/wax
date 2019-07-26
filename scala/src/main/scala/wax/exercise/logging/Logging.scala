package wax.exercise.logging

import java.io.FileOutputStream

import cats.effect.IO
import cats.syntax.monoid._
import cats.kernel.Monoid

/*

Your task is to:

1. Implement Monoid[Unit] instance.
2. Implement Monoid[A => B] instance.
3. Make LoggingSpec green.

 */

object Main extends App {
  import Logging._

  def consoleLogger: IO[Logger] = IO { input =>
    IO {
      print(input)
    }
  }

  def fileLogger(filePath: String): IO[Logger] = IO {
    val stream = new FileOutputStream(filePath)
    input => {
      IO {
        stream.write(input.getBytes)
      }
    }
  }

  val program: IO[Unit] = for {
    logger <- consoleLogger |+| fileLogger("logging.log")
    _      <- logger("Map Reduce\n")
    _      <- logger("Reduces Maps\n")
    _      <- logger("Map Reduce\n")
    _      <- logger("Map Reduce\n")
    _      <- logger("\n")
    _      <- logger("In parallel\n")
    _      <- logger("And not\n")
  } yield ()

  program.unsafeRunSync()
}

object Logging {
  type Logger = String => IO[Unit]

  implicit val monoidUnit: Monoid[Unit] = ???

  implicit def monoidFunction[A, B : Monoid]: Monoid[A => B] = ???
  
  implicit def monoidIo[A: Monoid]: Monoid[IO[A]] = IO.ioMonoid
}
