package oss.ggiussi.cappio.ui.levels

import japgolly.scalajs.react.vdom.VdomElement
import oss.ggiussi.cappio.core.Level


trait LevelT[S] {

  def level(): Level[S]

  // This should be a list of conditions so I could show in the ui if each condition holds or not

  def description: String = "Empty description"

}
