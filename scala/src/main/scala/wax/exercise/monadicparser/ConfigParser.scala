package wax.exercise.monadicparser

import cats.implicits._
import cats.{Alternative, Applicative, Functor}

import scala.language.higherKinds

case class HostConfig(hostType: String, host: String)

object ConfigParser {
  def parseConfig(env: String, cfg: String): HostConfig = ???
}

sealed trait ParserResult[A]
case class ParserSuccess[A](input: String, value: A) extends ParserResult[A]
case class ParserFailure[A]() extends ParserResult[A]

case class Parser[A](parse: String => ParserResult[A]) {
  def run(input: String): Either[String, A] = parse(input) match {
    case ParserFailure()                   => Left("parser error")
    case ParserSuccess(s, _) if s.nonEmpty => Left("parser did not consume entire stream: '" ++ s ++ "'")
    case ParserSuccess(_, v)               => Right(v)
  }

  def unsafeRun(input: String): A = run(input).left.map(e => throw new RuntimeException(e)).toTry.get
}

object Parser {
  implicit val parserResultFunctor: Functor[ParserResult] = ???

  implicit val parserFunctor: Functor[Parser] = ???

  implicit val parserApplicative: Applicative[Parser] = ???

  implicit val parserAlternative: Alternative[Parser] = ???

  def satisfy(pred: Char => Boolean): Parser[Char] = Parser { s =>
    if (s.nonEmpty && pred(s.head)) ParserSuccess(s.tail, s.head)
    else ParserFailure()
  }

  def char(a: Char): Parser[Char] = satisfy(_ == a)

  def notChar(a: Char): Parser[Char] = ???

  def anyChar: Parser[Char] = ???

  def space: Parser[Char] = char(' ')

  def oneOf(chars: Seq[Char]): Parser[Char] = satisfy(chars.contains)

  def digit: Parser[Char] = satisfy(_.isDigit)

  def letterOrDigit: Parser[Char] = satisfy(_.isLetterOrDigit)

  // TODO: consider asking people to implement this parser
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
