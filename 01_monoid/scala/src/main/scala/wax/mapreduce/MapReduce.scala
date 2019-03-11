package wax

import java.io.File
import java.time
import java.time.LocalDateTime.now

import cats.Monoid

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.io.Source

object Runner extends App {
  val start = now()


  /*
    Your code here
   */


  val end = now()
  println("Total seconds passed: " + time.Duration.between(start, end).getSeconds)
}

object MapReduce {

  def mapReduce[A, T: Monoid](m: List[A])(f: A => T): T = {
    m.map(f).foldLeft(Monoid.empty)(Monoid.combine[T])
  }

  def mapReducePar[A, T: Monoid](m: List[A])(f: A => T): T = {
    val empty = Future.successful(Monoid.empty)
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

object FileProcessor {
  def process[T: Monoid](f: String => T)(file: File): T = MapReduce.mapReducePar(readTokens(file))(f)
  def processMany[T: Monoid](f: String => T)(files: List[File]): T = MapReduce.mapReducePar(files)(process(f))
  def readTokens(file:File): List[String] = Source.fromFile(file).getLines.flatMap(tokenize).toList
  private def tokenize(str: String): List[String] =
    str.toLowerCase
      .replace("\n", " ")
      .replace("\t", " ")
      .replaceAll("""[\p{Punct}]""", "")
      .split(" ").toList
}