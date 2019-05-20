package oss.giussi.cappio.impl

import oss.giussi.cappio.ProcessId

package object time {

  def maxrank(alive: Set[ProcessId]) = alive.minBy(_.id)

}
