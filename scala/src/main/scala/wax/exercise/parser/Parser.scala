package wax.exercise.parser

import cats._
import cats.implicits._

import scala.language.higherKinds

sealed trait ParserResult[A]
case class ParserSuccess[A](input: String, value: A) extends ParserResult[A]
case class ParserFailure[A]() extends ParserResult[A]

case class Parser[A](parse : String => ParserResult[A])

object Parser {
  implicit val parserResultFunctor: Functor[ParserResult] = new Functor[ParserResult] {
    override def map[A, B](fa: ParserResult[A])(f: A => B): ParserResult[B] = fa match {
      case ParserFailure() => ParserFailure()
      case ParserSuccess(input, value) => ParserSuccess(input, f(value))
    }
  }

  implicit val parserFunctor: Functor[Parser] = new Functor[Parser] {
    override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] = Parser(s => fa.parse(s).fmap(f))
  }

  implicit val parserApplicative: Applicative[Parser] = new Applicative[Parser] {
    override def pure[A](x: A): Parser[A] = Parser(s => ParserSuccess(s, x))

    override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = Parser(s =>
      ff.parse(s) match {
        case ParserFailure() => ParserFailure()
        case ParserSuccess(s1, f) => fa.parse(s1).fmap(f)
      }
    )
  }

  implicit val parserAlternative: Alternative[Parser] = new Alternative[Parser] {
    override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = parserApplicative.ap(ff)(fa)

    override def empty[A]: Parser[A] = Parser(_ => ParserFailure())

    override def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] = Parser(s =>
      x.parse(s) match {
        case ParserFailure() => y.parse(s)
        case success => success
      }
    )

    override def pure[A](x: A): Parser[A] = parserApplicative.pure(x)
  }

  def satisfy(pred: Char => Boolean): Parser[Char] = Parser { s =>
    if (s.nonEmpty && pred(s.head)) ParserSuccess(s.tail, s.head)
    else ParserFailure()
  }

  def char(a: Char): Parser[Char] = satisfy(_ == a)

  def notChar: Parser[Char] = ???

  def anyChar: Parser[Char] = ???

  def space: Parser[Char] = char(' ')

  def oneOf(chars: Seq[Char]): Parser[Char] = satisfy(chars.contains)

  def digit: Parser[Char] = satisfy(_.isDigit)

  def letterOrDigit: Parser[Char] = satisfy(_.isLetterOrDigit)

  def number: Parser[Int] = {
    val sign = string("-") <+> "".pure[Parser]
    val digits = some(digit).fmap(_.mkString)
    val number = sign.fmap[String => String](a => b => a ++ b) <*> digits
    number.fmap(_.toInt)
  }

  def string(str: String): Parser[String] =
    if (str.isEmpty) str.pure[Parser]
    else char(str.head) *> string(str.tail) *> str.pure[Parser]

  def token[A](parser: Parser[A]): Parser[A] =
    many(space) *> parser <* many(space)

  def many[A](parser: Parser[A]): Parser[List[A]] = Parser(s =>
    parser.parse(s) match {
      case ParserFailure() => ParserSuccess(s, List.empty)
      case ParserSuccess(s1, v) => many(parser).parse(s1).fmap(xs => v :: xs)
    }
  )

  def some[A](parser: Parser[A]): Parser[List[A]] = Parser(s =>
    parser.parse(s) match {
      case ParserFailure() => ParserFailure()
      case ParserSuccess(s1, v) => many(parser).parse(s1).fmap(xs => v :: xs)
    }
  )
}
