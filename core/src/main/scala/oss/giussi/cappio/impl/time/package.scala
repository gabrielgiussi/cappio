package oss.giussi.cappio.impl

import oss.giussi.cappio.ProcessId

package object time {

  sealed trait HeartbeatMsg

  case object HeartbeatRequest extends HeartbeatMsg

  case object HeartbeatReply extends HeartbeatMsg

  case class Crashed(id: ProcessId)


  def maxrank(alive: Set[ProcessId]) = alive.minBy(_.id)

}
