package wax.exercise.logging

import java.io.FileOutputStream

import cats.effect.IO
import cats.syntax.monoid._
import cats.kernel.Monoid

object Logging extends App {

  type Logger = String => IO[Unit]

  implicit val monoidUnit: Monoid[Unit] = ???

  implicit def monoidFunction[A, B : Monoid]: Monoid[A => B] = ???

  implicit def monoidIo[A: Monoid]: Monoid[IO[A]] = IO.ioMonoid

  def consoleLogger: IO[Logger] = IO { input =>
    IO {
      print(input)
    }
  }

  def fileLogger(filePath: String): IO[Logger] = IO {
    println("Debug: create file handler")
    val stream = new FileOutputStream(filePath)
    input => IO(stream.write(input.getBytes))
  }

  def someApp(logger: Logger): IO[Unit] = for {
    input <- IO(scala.io.StdIn.readLine)
    _     <- logger(s"User input: $input\n")
    _     <- someApp(logger)
  } yield ()

  val program: IO[Unit] = for {
    logger <- consoleLogger |+| fileLogger("logging.log")
    _      <- someApp(logger)
  } yield ()

  program.unsafeRunSync()
}
