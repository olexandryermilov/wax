package wax.intro.typeclass

import wax.intro.proprietary.{Cockatrice, Element}

trait Introducible[A] {
  def introduce(a: A): String
}

object Introducible {
  def apply[A: Introducible]: Introducible[A] = implicitly[Introducible[A]]

  implicit val introducibleCockatrice: Introducible[Cockatrice] = (a: Cockatrice) => {
    s"Haha. I am a ${a.element.shortName} cockatrice of level ${a.level}."
  }
}

case class Hero(name: String, job: String, level: Int)

object Hero {
  implicit val introducibleHero: Introducible[Hero] = (a: Hero) => {
    import a._
    s"Hi! My name is $name. I am $level level $job."
  }
}

object GameExplicit extends App {
  def introduce[A](creature: A)(ev: Introducible[A]): Unit = println(ev.introduce(creature))

  introduce(Hero(name = "Valik", job = "Black Mage", level = 20))(Hero.introducibleHero)
  introduce(Cockatrice(level = 666, element = Element.Fire))(Introducible.introducibleCockatrice)
}

object GameImplicit extends App {
  def introduce[A](creature: A)(implicit ev: Introducible[A]): Unit = println(ev.introduce(creature))

  introduce(Hero(name = "Valik", job = "Black Mage", level = 20))
  introduce(Cockatrice(level = 666, element = Element.Fire))
}

object GameSummoner  extends App {
  def introduce[A: Introducible](creature: A): Unit = println(Introducible[A].introduce(creature))

  introduce(Hero(name = "Valik", job = "Black Mage", level = 20))
  introduce(Cockatrice(level = 666, element = Element.Fire))
}
