package wax.intro.wrapper

import wax.intro.proprietary.{Cockatrice, Element}

trait Introducible {
  def introduce(): String
}

trait HasTitle {
  def title(): String
}

case class Hero(name: String, job: String, level: Int) extends Introducible with HasTitle {
  override def introduce(): String = s"Hi! My name is $name. I am $level level $job."

  override def title(): String = name
}

case class Monster(name: String, level: Int) extends Introducible with HasTitle {
  override def introduce(): String = s"Lok-tar ogar! Me be $name. Me be strong. Level $level strong!"

  override def title(): String = name
}

case class CockatriceWrapper(cockatrice: Cockatrice) extends Introducible with HasTitle {
  override def introduce(): String =
    s"Haha. I am a ${cockatrice.element.shortName} cockatrice of level ${cockatrice.level}."

  override def title(): String = "Cockatrice"
}

object GameSimple extends App {
  def introduce(creature: Introducible): Unit = println(creature.introduce())

  val player = Hero(name = "Valik", job = "Black Mage", level = 20)
  val someRandomMonster = Monster(name = "Garrosh", level = 105)
  val cockatrice = CockatriceWrapper(Cockatrice(level = 666, element = Element.Fire))

  introduce(player)
  introduce(someRandomMonster)
  introduce(cockatrice)
}

object GameWithTitles extends App {
  def say(creature: HasTitle, message: String): Unit =
    println(s"[${creature.title()}]: $message")

  def introduce[A <: Introducible with HasTitle](creature: A): Unit =
    say(creature, creature.introduce())

  val player = Hero(name = "Valik", job = "Black Mage", level = 20)
  val someRandomMonster = Monster(name = "Garrosh", level = 105)
  val cockatrice = CockatriceWrapper(Cockatrice(level = 666, element = Element.Fire))

  introduce(player)
  introduce(someRandomMonster)
  introduce(cockatrice)
}
