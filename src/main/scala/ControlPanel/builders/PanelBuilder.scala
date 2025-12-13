package ControlPanel.builders

import scala.collection.immutable.LazyList
import scala.util.Random
import ControlPanel.elements._
import ControlPanel.model.ControlPanell

object PanelBuilder {

  def buildRandomPanel(width: Int, height: Int)(implicit random: Random): ControlPanell = {
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
      case (conn, buttonCoord) =>
        val (bx, by) = buttonCoord
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

    ControlPanell(width, height, updatedElements, connections)
  }
}