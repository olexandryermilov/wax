package wax.exercise.mapreduce

import java.io.File
import java.util.concurrent.Executors

import cats.Monoid
import cats.effect.{ContextShift, IO}
import wax.utils._

import scala.concurrent.ExecutionContext
import scala.io.Source

object Main extends App {

  import MapReduce._

  def job(strategy: Strategy): Result[Int] = mapReduce(strategy)(???) {
    ???
  }

  println("Par")
  val resPar = Benchmark(job(Par))
  println(resPar)

  println()

  println("Seq")
  val resSeq = Benchmark(job(Seq))
  println(resSeq)
}

object MapReduce {
  type Result[V] = Map[String, V]

  implicit val monoid: Monoid[Result[Int]] = new Monoid[Result[Int]] {
    override def empty: Result[Int] = ???

    override def combine(x: Result[Int], y: Result[Int]): Result[Int] = ???
  }

  val numCores: Int = Runtime.getRuntime.availableProcessors
  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.fromExecutor(Executors.newWorkStealingPool()))

  sealed trait Strategy
  object Seq extends Strategy
  object Par extends Strategy

  def mapReduce[A, T: Monoid](strategy: Strategy)(m: List[A])(f: A => T): T = strategy match {
    case Seq => seq(m)(f)
    case Par => par(m)(f)
  }

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
      .split(" ")
      .map(_.trim)
      .filterNot(_.isEmpty)
      .toList

  def readTokens(file: File): List[String] = Source.fromFile(file).getLines.flatMap(tokenize).toList
}
