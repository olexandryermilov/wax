package wax

import java.io.File
import java.time
import java.time.LocalTime

import cats.Monoid
import wax.util._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.io.Source

object MapReduce {

  def mapReduce[A, T: Monoid](m: List[A])(f: A => T): T = {
    m.map(f).foldLeft(Monoid.empty)(Monoid.combine[T])
  }

  def mapReducePar[A, T: Monoid](m: List[A])(f: A => T): T = {
    val empty = Future.successful(Monoid.empty)
    def reduce(m: List[Future[T]]): Future[T] = m match {
      case Nil => empty
      case h :: Nil => h
      case _ =>
        val nl = m.grouped(2).map {
          case Nil => empty
          case h :: Nil => h
          case h1 :: h2 :: Nil =>
            for {
              h1v <- h1
              h2v <- h2
            } yield Monoid.combine(h1v, h2v)
        }.toList
        reduce(nl)
    }
    Await.result(reduce(m.map(f).map(Future.successful)), Duration.Inf)
  }

}

object FileProcessor extends App {
  def process[T: Monoid](f: String => T)(path: File): T = MapReduce.mapReducePar(readTokens(path))(f)

  def processMany[T: Monoid](f: String => T)(paths: List[File]): T = MapReduce.mapReducePar(paths)(process(f))
}

object Appchik extends App {
  implicit val monoid = new Monoid[Map[String, Int]] {
    override def empty: Map[String, Int] = Map.empty

    override def combine(x: Map[String, Int],
                         y: Map[String, Int]): Map[String, Int] = {
      val keyset = x.keySet ++ y.keySet
      keyset.map(k => k -> {x.getOrElse(k, 0) + y.getOrElse(k, 0)}).toMap
    }
  }

  val files = new File(this.getClass.getClassLoader.getResource("books").toURI)
    .listFiles()
    .flatMap(_.listFiles())
    .toList
//    .take(10)

  runWithTiming {
    FileProcessor.processMany(str => Map(str -> 1))(files)
  }
}

//Word count (most used word longer that 5 letters)
//Inverted index (3-rd interview quiz)
//Count distinct
//Count distinct (HyperLogLog)
//Longest word

object util {
  def readTokens(file:File): List[String] = Source.fromFile(file).getLines.flatMap(tokenize).toList

  def tokenize(str: String): List[String] =
    str.toLowerCase
      .replace("\n", " ")
      .replace("\t", " ")
      .replaceAll("""[\p{Punct}]""", "")
      .split(" ").toList

  def runWithTiming[T](a: => T): T = {
    val start = LocalTime.now
    def date = LocalTime.now.toString
    def pr(s: String) = println(date + ". " + s)

    pr("MapReduce start")

    val res: T = a

    val end: LocalTime = LocalTime.now
    pr("MapReduce end. Printing...")
    pr(res + "")
    println("Total seconds passed: " + time.Duration.between(start, end).getSeconds)

    res
  }

}