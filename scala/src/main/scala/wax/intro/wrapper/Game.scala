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

case class Orc(name: String, level: Int) extends Introducible with HasTitle {
  override def introduce(): String = s"Lok-tar ogar! Me be $name. Me be strong. Level $level strong!"

  override def title(): String = name
}

case class Ooze(level: Int) extends Introducible with HasTitle {
  override def introduce(): String = 1.to(level).map(_=>"brlup").mkString("-")

  override def title(): String = "Ooze"
}

case class CockatriceWrapper(cockatrice: Cockatrice) extends Introducible with HasTitle {
  override def introduce(): String =
    s"Haha. I am a ${cockatrice.element.shortName} cockatrice of level ${cockatrice.level}."

  override def title(): String = "Cockatrice"
}

object GameSimple extends App {
  val player = Hero(name = "Valik", job = "Black Mage", level = 20)
  val orc = Orc(name = "Garrosh", level = 105)
  val ooze = Ooze(level = 2)
  val cockatrice = CockatriceWrapper(Cockatrice(level = 666, element = Element.Fire))

  def introduce(creatute: Introducible): Unit = {
    // some real shit animations
    println(creatute.introduce())
    // some real shit animations
  }

  introduce(player)
  introduce(orc)
  introduce(ooze)
  introduce(cockatrice)
}

object GameWithTitles extends App {
  val player = Hero(name = "Valik", job = "Black Mage", level = 20)
  val orc = Orc(name = "Garrosh", level = 105)
  val ooze = Ooze(level = 2)
  val cockatrice = CockatriceWrapper(Cockatrice(level = 666, element = Element.Fire))

  def say(creature: HasTitle, message: String): Unit =
    println(s"[${creature.title()}]: $message")

  def introduce[A <: Introducible with HasTitle](creature: A): Unit =
    say(creature, creature.introduce())

  introduce(player)
  introduce(orc)
  introduce(ooze)
  introduce(cockatrice)
}
