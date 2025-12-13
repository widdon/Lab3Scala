package ControlPanel

import scala.util.Random
import scala.collection.immutable.LazyList
import ControlPanel.builders.PanelBuilder
import ControlPanel.util.InputProcessor
import ControlPanel.model.ControlPanell

object Main {

  // Основной игровой цикл с ленивым потоком ввода
  def Loop(panel: ControlPanell): Unit = {
    println("\nСгенерирована панель управления:")
    println(panel.display)
    println("\nВведите координаты кнопки (x y) или -1 для выхода:")

    // Создаем бесконечный ленивый поток ввода
    val inputStream: LazyList[String] = LazyList.continually(scala.io.StdIn.readLine())

    // Обрабатываем команды из ленивого потока
    InputProcessor.processCommands(panel, inputStream)
  }

  def main(args: Array[String]): Unit = {
    implicit val random: Random = new Random()

    println("Введите размеры панели (ширина высота):")

    val dimensions = scala.io.StdIn.readLine().trim

    dimensions.split("\\s+") match {
      case Array(widthStr, heightStr) =>
        try {
          val width = widthStr.toInt
          val height = heightStr.toInt

          // Проверяем, что размеры положительные
          if (width > 0 && height > 0) {
            val panel = PanelBuilder.buildRandomPanel(width, height)
            Loop(panel)
          } else {
            println("Размеры должны быть положительными числами")
          }
        } catch {
          case _: NumberFormatException =>
            println("Введите два положительных числа через пробел")
        }
      case _ =>
        println("Введите два числа через пробел (ширина высота)")
    }
  }
}