package wax

import java.io.File
import java.time
import java.time.LocalTime

import cats.Monoid
import wax.util._

import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.global
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.Source

object MapReduce {

  def mapReduce[A, T: Monoid](m: List[A])(map: A => T): T = {
    m.map(map).foldLeft(Monoid.empty)(Monoid.combine[T])
  }

  def mapReducePar[A, T: Monoid](m: List[A])(map: A => T): Future[T] = {
    def reduce(m: List[Future[T]]): Future[T] = m match {
      case Nil => Future.successful(Monoid.empty)
      case h :: Nil => h
      case _ =>
        val nl = m.grouped(2).map {
          case Nil => Future.successful(Monoid.empty)
          case h :: Nil => h
          case h1 :: h2 :: Nil =>
            for {
              h1v <- h1
              h2v <- h2
            } yield Monoid.combine(h1v, h2v)
        }.toList
        reduce(nl)
    }
    reduce(m.map(map).map(Future.successful))
  }

}

//Word frequency
object WordFrequencyCalc extends App {
  val words = Source.fromResource("asimov.txt")
                    .getLines
                    .flatMap(tokenize)
                    .toList

  private val monoid = new Monoid[Map[String, Int]] {
    override def empty: Map[String, Int] = Map.empty

    override def combine(x: Map[String, Int],
                         y: Map[String, Int]): Map[String, Int] =
      x ++ y.map { case (key, value) => key -> (value + x.getOrElse(key, 0)) }
  }
  val res = runWithTiming {
    val resF = MapReduce.mapReducePar(words)(w => Map(w -> 1))(monoid)
    Await.result(resF, Duration.Inf)
//    val res = MapReduce.mapReduce(words)(w => Map(w -> 1))(monoid)
  }
  print(res.toList.sortBy(_._2).reverse)
}

//Inverted index (3-rd interview quiz)
object InvertedIndexBuilder extends App {
  //load a bunch of files
  val fileToToken = new File("inverted_index").listFiles().toList.flatMap(f => tokenizeFile(f).map(t => f.getName -> t))
  val monoid = new Monoid[Map[String, Set[String]]] {
    override def empty: Map[String, Set[String]] = Map.empty

    override def combine(x: Map[String, Set[String]],
                         y: Map[String, Set[String]]): Map[String, Set[String]] =
      x ++ y.map { case (key, value) => key -> (value ++ x.getOrElse(key, Set.empty)) }
  }
  runWithTiming{
//    MapReduce.mapReduce(fileToToken){ case (f,t) => Map(t -> Set(f)) }(monoid)
    Await.result(MapReduce.mapReducePar(fileToToken){ case (f,t) => Map(t -> Set(f)) }(monoid), Duration.Inf)
  }
}

//Friends
//Get friends graph of all the wix kiev employees?
//object FriendGraphBuilder extends App {
  //load the friend mappings
  //mapreduce. Map = (Person, Person) -> Set(Person) mappings where people in tuples are sorted by name, reduce
//}

object util {
  def tokenizeFile(file:File): List[String] = Source.fromFile(file).getLines().flatMap(tokenize).toList
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
    val words = Source.fromResource("asimov.txt")
      .getLines
      .flatMap(tokenize)
      .toList

    pr("MapReduce start")

    val res = a()

    val end: LocalTime = LocalTime.now
    pr("MapReduce end. Printing...")
    println("Total seconds passed: " + time.Duration.between(start, end).getSeconds)

    res
  }

}

//class MapMonoid[T: Monoid] extends Monoid[Map[String, T]] {
//  override def empty: Map[String, T] = ???
//
//  override def combine(x: Map[String, T],
//                       y: Map[String, T]): Map[String, T] = ???
//}

