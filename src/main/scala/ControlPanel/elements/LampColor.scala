package ControlPanel.elements

import scala.util.Random

sealed trait LampColor {
  def code: String
}

object LampColor {
  case object Red extends LampColor { val code = "к" }
  case object Green extends LampColor { val code = "з" }
  case object Blue extends LampColor { val code = "с" }
  case object Yellow extends LampColor { val code = "ж" }

  val values: List[LampColor] = List(Red, Green, Blue, Yellow)

  def randomColor(random: Random): LampColor =
    values(random.nextInt(values.length))
}