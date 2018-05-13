import java.io.{File, FileOutputStream, PrintWriter}

import cats.effect.IO
import cats.implicits._

object Logging {
  val readLn = IO(scala.io.StdIn.readLine)

  type Logger = String => IO[Unit]

  def consoleLogger: IO[Logger] = IO { input =>
    IO {
      print(input)
    }
  }

  def fileLogger(filePath: String): IO[Logger] = IO {
    println("Debug: create file handler")
    val stream = new FileOutputStream(filePath)
    input =>
      IO {
        stream.write(input.getBytes)
      }
  }

  def main(args: Array[String]): Unit = program.unsafeRunSync()

  val program: IO[Unit] = for {
    logger <- consoleLogger combine fileLogger("logging.log")
    _ <- run(logger)
  } yield ()

  def run(logger: Logger): IO[Unit] = for {
    input <- readLn
    _ <- logger(s"User input: $input\n")
    _ <- run(logger)
  } yield ()
}
