package wax.mapreduce

import java.io.File
import java.time
import java.time.LocalDateTime.now

import cats.Monoid
import cats.effect.IO
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.io.Source

object Runner extends App {
  val start = now()
  implicit val monoid = new Monoid[Map[String, Int]] {
    override def empty: Map[String, Int] = Map.empty

    override def combine(x: Map[String, Int],
                         y: Map[String, Int]): Map[String, Int] = {
      val keyset = x.keySet ++ y.keySet
      keyset.map(k => k -> {x.getOrElse(k, 0) + y.getOrElse(k, 0)}).toMap
    }
  }

  val files = FileUtils.authorBooks("asimov").head :: Nil
  val strings = files.flatMap(FileUtils.readTokens)
  println("ZDELANO")
  val res = MapReduce.par(strings)(t => Map(t -> 1))

  val end = now()
  println("Total seconds passed: " + time.Duration.between(start, end).getSeconds)
  println(s"res: $res")
}

object MapReduce {

  private val numCores = Runtime.getRuntime.availableProcessors
  println(s"Cores: $numCores")
  def seq[A, T: Monoid](m: List[A])(f: A => T): T =
    m.map(f).foldLeft(Monoid.empty[T])(Monoid.combine[T])

  def par[A, T: Monoid](m: List[A])(f: A => T): T = {
    import cats.implicits._
    val groupSize = (1.0 * m.size / numCores).ceil.toInt
    m.grouped(groupSize).toList
     .parTraverse(a => IO(a.map(f).foldLeft(Monoid.empty[T])(Monoid.combine[T])))
     .map(_.foldLeft(Monoid.empty[T])(Monoid.combine[T]))
     .unsafeRunSync()
  }

  def mapReducePar[A, T: Monoid](m: List[A])(f: A => T): T = {
    val empty = Future.successful(Monoid.empty[T])
    def reduce(m: List[Future[T]]): Future[T] = m match {
      case Nil      => empty
      case a :: Nil => a
      case _        => reduce {
        m.grouped(2).map {
          case Nil           => empty
          case a :: Nil      => a
          case a :: b :: Nil =>
            for {
              av <- a
              bv <- b
            } yield Monoid.combine(av, bv)
        }.toList
      }
    }
    Await.result(reduce(m.map(f).map(Future.successful)), Duration.Inf)
  }
}

object FileUtils {
  private def file(path: String) = new File(this.getClass.getClassLoader.getResource(path).toURI)
  def allAuthors: List[String] = file("books").listFiles().map(_.getName).toList
  def allBooks: List[File] = allAuthors.map(author => file(s"books/$author")).flatMap(_.listFiles())
  def authorBooks(author: String): List[File] = file(s"books/$author").listFiles().toList

  private def tokenize(str: String): List[String] =
    str.toLowerCase
      .replace("\n", " ")
      .replace("\t", " ")
      .replaceAll("""[\p{Punct}]""", "")
      .split(" ").toList
  def readTokens(file: File): List[String] = Source.fromFile(file).getLines.flatMap(tokenize).toList
}