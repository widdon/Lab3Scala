package ControlPanel.elements

sealed trait PanelElement {
  def x: Int
  def y: Int
  def display: String
}

case class Button(
                   x: Int,
                   y: Int,
                   state: ElementState = ElementState.Released,
                   connectedLamps: Set[(Int, Int)] = Set.empty
                 ) extends PanelElement {

  def press: Button = copy(state = ElementState.Pressed)

  // Новый метод: отжать кнопку
  def release: Button = copy(state = ElementState.Released)

  def addLamp(coord: (Int, Int)): Button =
    copy(connectedLamps = connectedLamps + coord)

  override def display: String = state match {
    case ElementState.Pressed => "o"
    case ElementState.Released => "O"
    case ElementState.Active => "?" // Не должно случиться
    case ElementState.Inactive => "?" // Не должно случиться
  }
}

case class Lamp(
                 x: Int,
                 y: Int,
                 color: LampColor,
                 state: ElementState = ElementState.Inactive
               ) extends PanelElement {

  def activate: Lamp = copy(state = ElementState.Active)
  def deactivate: Lamp = copy(state = ElementState.Inactive)

  override def display: String = state match {
    case ElementState.Active => s"Л_${color.code}"
    case ElementState.Inactive => "Л"
    case ElementState.Pressed => "?" // Не должно случиться
    case ElementState.Released => "?" // Не должно случиться
  }
}
