package oss.giussi.cappio.ui

import com.raquo.airstream.eventbus.EventBus
import oss.giussi.cappio.Mod
import oss.giussi.cappio.ui.levels.Op

class AppStore[M <: Mod] {

  val ops = new EventBus[Op[M]]

}
