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

  def mapReduce[A, T: Monoid](m: List[A])(map: A => T): T = {
    m.map(map).foldLeft(Monoid.empty)(Monoid.combine[T])
  }

  def mapReducePar[A, T: Monoid](m: List[A])(map: A => T): T = {
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
    Await.result(reduce(m.map(map).map(Future.successful)), Duration.Inf)
  }

}

object FileProcessor extends App {
  def process[T: Monoid](f: String => T)(path: File): T = MapReduce.mapReduce(readTokens(path))(f)

  def processMany[T: Monoid](f: String => T)(paths: List[File]): T = MapReduce.mapReduce(paths)(process(f))
}

object Appchik extends App {
  implicit val monoid = new Monoid[Map[String, Int]] {
    override def empty: Map[String, Int] = Map.empty

    override def combine(x: Map[String, Int],
                         y: Map[String, Int]): Map[String, Int] =
      x ++ y.map { case (key, value) => key -> (value + x.getOrElse(key, 0)) }
  }

  val files = new File(this.getClass.getClassLoader.getResource("books").toURI)
    .listFiles()
    .flatMap(_.listFiles())
    .toList

  runWithTiming {
    FileProcessor.processMany(str => Map(str -> 1))(files)
  }
}

//Inverted index (3-rd interview quiz)
object InvertedIndexBuilder extends App {

  //load a bunch of files
  val fileToToken = new File("inverted_index").listFiles().toList.flatMap(f => readTokens(f).map(t => f.getName -> t))
  val monoid = new Monoid[Map[String, Set[String]]] {
    override def empty: Map[String, Set[String]] = Map.empty

    override def combine(x: Map[String, Set[String]],
                         y: Map[String, Set[String]]): Map[String, Set[String]] =
      x ++ y.map { case (key, value) => key -> (value ++ x.getOrElse(key, Set.empty)) }
  }
  runWithTiming{
    MapReduce.mapReduce(fileToToken){ case (f,t) => Map(t -> Set(f)) }(monoid)
//    Await.result(MapReduce.mapReducePar(fileToToken){ case (f,t) => Map(t -> Set(f)) }(monoid), Duration.Inf)
  }
}

//Friends
//Get friends graph of all the wix kiev employees?
//object FriendGraphBuilder extends App {
  //load the friend mappings
  //mapreduce. Map = (Person, Person) -> Set(Person) mappings where people in tuples are sorted by name, reduce
//}

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
    println("Total seconds passed: " + time.Duration.between(start, end).getSeconds)

    res
  }

}