package oss.ggiussi.cappio.ui.levels

import oss.ggiussi.cappio.core.Level
import oss.ggiussi.cappio.ui.app.ActionSelectionProps


trait LevelT[S] {

  def level(): Level[S]

  def description: String = "Empty description"

}
