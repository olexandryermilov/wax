package wax.exercise.parser

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

object ParserSpec extends Properties("Parser") {
  property("char parser consumes first character of the string") = forAll { s: String =>
    if (s.isEmpty) Parser.char(' ').parse(s) == ParserFailure()
    else Parser.char(s.head).parse(s) == ParserSuccess(s.tail, s.head)
  }

  property("notChar parser consumes any character from the string except for specific") = forAll { (c: Char, s: String) =>
    if (s.isEmpty) Parser.notChar(' ').parse(s) == ParserFailure()
    else if (s.head == c) Parser.notChar(c).parse(s) == ParserFailure()
    else Parser.notChar(c).parse(s) == ParserSuccess(s.tail, s.head)
  }

  property("anyChar parser consumes any character from the string") = forAll { s: String =>
    if (s.isEmpty) Parser.anyChar.parse(s) == ParserFailure()
    else Parser.anyChar.parse(s) == ParserSuccess(s.tail, s.head)
  }

  property("number parser consumes number") = forAll { v: Int =>
    Parser.number.parse(v.toString) == ParserSuccess("", v)
  }

  property("token parser ignores spaces around") = forAll { (v: Int, spacesBefore: Int, spacesAfter: Int ) =>
    val s = (" " * (spacesBefore.abs % 20)) ++ v.toString ++ (" " * (spacesAfter.abs % 20))
    Parser.token(Parser.number).parse(s) == ParserSuccess("", v)
  }
}
