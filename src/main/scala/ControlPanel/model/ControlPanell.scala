package ControlPanel.model

import ControlPanel.elements._

case class ControlPanell(
                         width: Int,
                         height: Int,
                         elements: Map[(Int, Int), PanelElement],
                         connections: Map[(Int, Int), Set[(Int, Int)]]
                       ) {

  def getElement(x: Int, y: Int): Option[PanelElement] =
    elements.get((x, y))

  def updateElement(x: Int, y: Int, element: PanelElement): ControlPanell =
    copy(elements = elements.updated((x, y), element))

  // Метод для переключения кнопки (нажать/отжать)
  def toggleButton(x: Int, y: Int): ControlPanell = {
    getElement(x, y) match {
      case Some(button: Button) =>
        // Определяем, какое действие выполнить
        val (updatedButton, shouldActivate) = button.state match {
          case ElementState.Released =>
            (button.press, true) // Нажимаем кнопку, лампы нужно включить
          case ElementState.Pressed =>
            (button.release, false) // Отжимаем кнопку, лампы нужно выключить
          case _ => (button, false) // Не должно случиться
        }

        val updatedPanel = updateElement(x, y, updatedButton)

        // Обрабатываем связанные лампы
        connections.get((x, y)) match {
          case Some(lampCoords) =>
            lampCoords.foldLeft(updatedPanel) { (panel, lampCoord) =>
              val (lx, ly) = lampCoord
              panel.getElement(lx, ly) match {
                case Some(lamp: Lamp) =>
                  val updatedLamp = if (shouldActivate) lamp.activate else lamp.deactivate
                  panel.updateElement(lx, ly, updatedLamp)
                case _ => panel
              }
            }
          case None => updatedPanel
        }
      case _ => this
    }
  }

  def toggleButtonWithCheck(x: Int, y: Int): (ControlPanell, String) = {
    getElement(x, y) match {
      case Some(button: Button) =>
        (toggleButton(x, y), "")
      case Some(_: Lamp) =>
        (this, "Здесь нет кнопки!")
      case None =>
        (this, "В этой ячейке нет элемента!")
    }
  }

  def display: String = {
    (0 until height).map { y =>
      (0 until width).map { x =>
        elements.get((x, y)).map(_.display).getOrElse(" ")
      }.mkString(" - ")
    }.mkString("\n")
  }
}