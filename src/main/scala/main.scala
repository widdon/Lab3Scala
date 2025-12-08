package ControlPanel

import scala.annotation.tailrec
import scala.collection.immutable.LazyList
import scala.util.Random

// ================ ADT ================
// Трейт для состояний элементов панели
sealed trait ElementState
case object Pressed extends ElementState
case object Released extends ElementState
case object Active extends ElementState
case object Inactive extends ElementState

// Трейт для цветов ламп с их кодами отображения
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

// Трейт для всех элементов панели
sealed trait PanelElement {
  def x: Int
  def y: Int
  def display: String
}

case class Button(
                   x: Int,
                   y: Int,
                   state: ElementState = Released,
                   connectedLamps: Set[(Int, Int)] = Set.empty
                 ) extends PanelElement {
  def press: Button = copy(state = Pressed)
  def addLamp(coord: (Int, Int)): Button = copy(connectedLamps = connectedLamps + coord)

  override def display: String = state match {
    case Pressed => "o"
    case Released => "O"
  }
}

case class Lamp(
                 x: Int,
                 y: Int,
                 color: LampColor,
                 state: ElementState = Inactive
               ) extends PanelElement {
  def activate: Lamp = copy(state = Active)
  def deactivate: Lamp = copy(state = Inactive)

  override def display: String = state match {
    case Active => s"Л_${color.code}"
    case Inactive => "Л"
  }
}

// ================ Панель управления ================
case class ControlPanel(
                         width: Int,
                         height: Int,
                         elements: Map[(Int, Int), PanelElement],
                         connections: Map[(Int, Int), Set[(Int, Int)]]
                       ) {
  def getElement(x: Int, y: Int): Option[PanelElement] =
    elements.get((x, y))

  def updateElement(x: Int, y: Int, element: PanelElement): ControlPanel =
    copy(elements = elements.updated((x, y), element))

  def pressButton(x: Int, y: Int): ControlPanel = {
    getElement(x, y) match {
      case Some(button: Button) =>
        val updatedButton = button.press
        val updatedPanel = updateElement(x, y, updatedButton)

        connections.get((x, y)) match {
          case Some(lampCoords) =>
            lampCoords.foldLeft(updatedPanel) { (panel, lampCoord) =>
              val (lx, ly) = lampCoord
              panel.getElement(lx, ly) match {
                case Some(lamp: Lamp) =>
                  panel.updateElement(lx, ly, lamp.activate)
                case _ => panel
              }
            }
          case None => updatedPanel
        }
      case _ => this
    }
  }

  // Метод нажатия кнопки с проверкой типа элемента и возвратом сообщения об ошибке
  def pressButtonWithCheck(x: Int, y: Int): (ControlPanel, String) = {
    getElement(x, y) match {
      case Some(button: Button) =>
        (pressButton(x, y), "")
      case Some(_: Lamp) =>
        (this, "Здесь нет кнопки!")
      case None =>
        (this, "В этой ячейке нет элемента!")
    }
  }

  // Метод для отображения панели
  def display: String = {
    (0 until height).map { y =>
      (0 until width).map { x =>
        elements.get((x, y)).map(_.display).getOrElse(" ")
      }.mkString(" - ")
    }.mkString("\n")
  }
}

// ================ Фабрика и билдер ================
object PanelBuilder {

  def buildRandomPanel(width: Int, height: Int)(implicit random: Random): ControlPanel = {
    // Ленивое создание координат всех ячеек
    val allCoords = LazyList.from(0 until height).flatMap(y =>
      LazyList.from(0 until width).map(x => (x, y))
    )

    // Создание элементов
    val (elements, buttonCoords, lampCoords) = allCoords.foldLeft(
      (Map.empty[(Int, Int), PanelElement], List.empty[(Int, Int)], List.empty[(Int, Int)])
    ) { case ((elems, buttons, lamps), (x, y)) =>
      if (random.nextBoolean()) {
        val button = Button(x, y)
        (elems.updated((x, y), button), (x, y) :: buttons, lamps)
      } else {
        val lamp = Lamp(x, y, LampColor.randomColor(random))
        (elems.updated((x, y), lamp), buttons, (x, y) :: lamps)
      }
    }

    // Создание связей
    val connections = buttonCoords.foldLeft(Map.empty[(Int, Int), Set[(Int, Int)]]) {
      case (conn, buttonCoord @ (bx, by)) =>
        val numLamps = random.nextInt(3) + 1
        val connectedLamps = random.shuffle(lampCoords).take(numLamps).toSet
        conn.updated(buttonCoord, connectedLamps)
    }

    // Обновление кнопок с информацией о связях
    val updatedElements = connections.foldLeft(elements) {
      case (elems, (buttonCoord, lampCoordsSet)) =>
        elems.get(buttonCoord) match {
          case Some(button: Button) =>
            val updatedButton = lampCoordsSet.foldLeft(button)(_.addLamp(_))
            elems.updated(buttonCoord, updatedButton)
          case _ => elems
        }
    }

    ControlPanel(width, height, updatedElements, connections)
  }
}

// ================ Основная программа ================
object Main {

  @tailrec
  def gameLoop(panel: ControlPanel): Unit = {
    println("\n" + panel.display)
    println("\nВведите координаты кнопки (x y) или -1 для выхода:")

    val input = scala.io.StdIn.readLine().trim

    if (input == "-1") return

    val (nextPanel, message) = input.split("\\s+") match {
      case Array(xStr, yStr) =>
        try {
          val x = xStr.toInt
          val y = yStr.toInt

          // Проверяем, что координаты в пределах панели
          if (x >= 0 && x < panel.width && y >= 0 && y < panel.height) {
            panel.pressButtonWithCheck(x, y)
          } else {
            (panel, s"Неверные координаты! Диапазон: x: 0-${panel.width - 1}, y: 0-${panel.height - 1}")
          }
        } catch {
          case _: NumberFormatException =>
            (panel, "Введите два числа через пробел")
        }
      case _ =>
        (panel, "Введите два числа через пробел")
    }

    if (message.nonEmpty) {
      println(message)
    }

    // Рекурсивный вызов с обновленной панелью (хвостовая рекурсия)
    gameLoop(nextPanel)
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
            println("\nСгенерирована панель управления:")
            gameLoop(panel)
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

// ================ Итератор для панели ================
object PanelIterator {
  def iterate(panel: ControlPanel): LazyList[PanelElement] = {
    for {
      y <- LazyList.from(0 until panel.height)
      x <- LazyList.from(0 until panel.width)
      elem <- panel.elements.get((x, y))
    } yield elem
  }
}

// ================ Компонент для группировки ================
case class PanelComponent(
                           x: Int,
                           y: Int,
                           children: List[PanelElement] = Nil
                         ) extends PanelElement {
  def add(element: PanelElement): PanelComponent =
    copy(children = element :: children)

  override def display: String =
    children.map(_.display).mkString("[", " ", "]")
}