package ControlPanel.util

import scala.annotation.tailrec
import scala.collection.immutable.LazyList
import ControlPanel.model.ControlPanell

object InputProcessor {

  // Функция для обработки одной команды
  private def processCommand(panel: ControlPanell, command: String): (ControlPanell, String) = {
    // Разбиваем команду на части
    val parts = command.trim.split("\\s+")

    parts match {
      case Array(xStr, yStr) => // Старая форма: "0 0" (по умолчанию toggle)
        processCoordinates(panel, xStr, yStr, toggle = true)

      case _ =>
        (panel, "Неверная команда! x y для переключения кнопки")
    }
  }

  // Обработка координат с указанием действия
  private def processCoordinates(panel: ControlPanell, xStr: String, yStr: String,
                                 toggle: Boolean = false): (ControlPanell, String) = {
    try {
      val x = xStr.toInt
      val y = yStr.toInt

      if (x >= 0 && x < panel.width && y >= 0 && y < panel.height) {
        if (toggle) {
          // Используем toggle для обратной совместимости
          panel.toggleButtonWithCheck(x, y)
        } else {
          (panel, "Не указано действие для кнопки!")
        }
      } else {
        (panel, s"Неверные координаты! Диапазон: x: 0-${panel.width - 1}, y: 0-${panel.height - 1}")
      }
    } catch {
      case _: NumberFormatException =>
        (panel, "Введите числа для координат!")
    }
  }

  @tailrec
  def processCommands(panel: ControlPanell, commands: LazyList[String]): Unit = {
    commands match {
      case LazyList() => // Поток пуст - завершаем работу
        println("Конец ввода")

      case command #:: restCommands =>
        val trimmedCommand = command.trim

        if (trimmedCommand == "-1") {
          println("Выход из программы")
        } else {
          // Обрабатываем команду
          val (nextPanel, message) = processCommand(panel, trimmedCommand)

          // Выводим сообщение об ошибке, если оно есть
          if (message.nonEmpty) {
            println(message)
          }

          // Если команда была успешной, показываем обновленную панель
          if (message.isEmpty) {
            println("\nТекущее состояние панели:")
            println(nextPanel.display)
          } else {
            // Если была ошибка, показываем ту же панель
            println("\nТекущее состояние панели:")
            println(panel.display)
          }

          // Рекурсивно обрабатываем оставшиеся команды
          println("\nВведите координаты кнопки (x y) или -1 для выхода:")
          processCommands(if (message.isEmpty) nextPanel else panel, restCommands)
        }
    }
  }

}
