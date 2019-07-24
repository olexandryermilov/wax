package wax.exercise.mapreduce

import java.io.File
import java.util.concurrent.Executors

import cats.Monoid
import cats.effect.{ContextShift, IO}
import wax.utils._

import scala.concurrent.ExecutionContext
import scala.io.Source

/*

Your task is to:

1. Impement Monoid instance for Result[A]
2. Make MapReduceSpec green
3. Implement job (so it returns the Map of all words amount of usages)
4. Run Main and see the results.

All books are located in the resources/mapreduce folder.

To make it easier, use the following helpers:

1. readTokens(file) to get the list of words from file
2. authorBooks("boris") will return two smol books (useful for tests)
3. authorBooks("asimov) just a next-level literature (only in terms of the size).
4. allBooks

 */

object MapReduce {
  //
  // Result[V] is an alias for Map[String, V].
  //
  // In the context of our task we will work with Result[Int], so it's a map where key is word from book and value is
  // a how many times that word was used.
  //
  type Result[V] = Map[String, V]

  //
  // TODO: implement Monoid instance for Result[Int]
  // Run MapReduceSpec to make sure that it's working correctly.
  //
  // In general you can provide a monoid instance for Result[A]. But Result[Int] is enough for this task.
  implicit val monoid: Monoid[Result[Int]] = ???

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

object Main extends App {

  import FileUtils._
  import MapReduce._

  // TODO: implement job
  def job(strategy: Strategy): Result[Int] = mapReduce(strategy)(???) {
    ???
  }

  // This is just a helper that gets first 10 words of length > 4 (because original result is too huge).
  def job_(strategy: Strategy): List[(String, Int)] = job(strategy)
    .toList
    .filter(_._1.length > 4)
    .sortBy(_._2)(implicitly[Ordering[Int]].reverse)
    .take(10)

  println("Par")
  val resPar = Benchmark(job_(Par))
  println(resPar)

  println()

  println("Seq")
  val resSeq = Benchmark(job_(Seq))
  println(resSeq)
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
