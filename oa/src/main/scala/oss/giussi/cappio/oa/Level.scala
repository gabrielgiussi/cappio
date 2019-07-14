package oss.giussi.cappio.oa

import scala.scalajs.js
import scala.scalajs.js.UndefOr

object Level {
  def level: Option[Int] = js.Dynamic.global.level.asInstanceOf[UndefOr[Int]].toOption
}
