package ControlPanel.elements

import scala.collection.immutable.LazyList
import ControlPanel.model.ControlPanell

object PanelIterator {
  def iterate(panel: ControlPanell): LazyList[PanelElement] = {
    for {
      y <- LazyList.from(0 until panel.height)
      x <- LazyList.from(0 until panel.width)
      elem <- panel.elements.get((x, y))
    } yield elem
  }
}