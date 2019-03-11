package wax

import java.io.FileOutputStream

import cats.effect.IO
import cats.kernel.Monoid

object Logging {
  val program: IO[Unit] = for {
    logger <- Monoid.combine(consoleLogger, fileLogger("logging.log"))
    _      <- run(logger)
  } yield ()

  def main(args: Array[String]): Unit = program.unsafeRunSync()

  type Logger = String => IO[Unit]

  implicit def monoid[A]: Monoid[IO[A]] = {
    /*
      Your code here
     */
    ???
  }

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

  def run(logger: Logger): IO[Unit] = for {
    input <- IO(scala.io.StdIn.readLine)
    _     <- logger(s"User input: $input\n")
    _     <- run(logger)
  } yield ()
}
