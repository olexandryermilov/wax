package wax.exercise.mapreduce

import java.io.File
import java.time
import java.time.LocalDateTime.now
import java.util.concurrent.Executors

import cats.{Functor, Monoid}
import cats.effect.{ContextShift, IO, Sync}

import scala.concurrent.ExecutionContext
import scala.io.Source

object MapReduceRunner extends App {
  def run[T](t: => T) = {
    val start = now()
    val res = t
    val end = now()
    println("Millis passed: " + time.Duration.between(start, end).toMillis)
    res
  }

  implicit val monoid: Monoid[Map[String, Int]] = ???

  run(???)

  trait FunctorOption extends Functor[Option] {
    def fmap[A, B](f: A => B)(fa: Option[A]) =
      fa match {
        case Some(a) => Some(f(a))
        case None => None
      }
  }
}

object MapReduce {
  val numCores = Runtime.getRuntime.availableProcessors
  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.fromExecutor(Executors.newWorkStealingPool()))

  def seq[A, T: Monoid](m: List[A])(f: A => T): T =
    m.map(f).foldLeft(Monoid.empty[T])(Monoid.combine[T])

  def par[A, T: Monoid](m: List[A])(f: A => T): T = {
    import cats.implicits._
    val groupSize = (1.0 * m.size / numCores).ceil.toInt
    m.grouped(groupSize).toList
     .parTraverse(a => IO(a.map(f).foldLeft(Monoid.empty[T])(Monoid.combine[T])))
     .map(_.combineAll)
     .unsafeRunSync()
  }
}

object FileUtils {
  private def file(path: String) = new File(this.getClass.getClassLoader.getResource(path).toURI)
  def allAuthors: List[String] = file("mapreduce/books").listFiles().map(_.getName).toList
  def allBooks: List[File] = allAuthors.map(author => file(s"mapreduce/books/$author")).flatMap(_.listFiles())
  def authorBooks(author: String): List[File] = file(s"mapreduce/books/$author").listFiles().toList

  private def tokenize(str: String): List[String] =
    str.toLowerCase
      .replace("\n", " ")
      .replace("\t", " ")
      .replaceAll("""[\p{Punct}]""", "")
      .split(" ").toList
  def readTokens(file: File): List[String] = Source.fromFile(file).getLines.flatMap(tokenize).toList
}
