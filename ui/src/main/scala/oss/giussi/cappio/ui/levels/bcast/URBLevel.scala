package oss.giussi.cappio.ui.levels.bcast

import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.{URBBcast, URBDeliver, URBMod}
import oss.giussi.cappio.ui.ActionSelection
import oss.giussi.cappio.ui.ActionSelection.payloadRequest
import oss.giussi.cappio.ui.levels.AbstractLevel
import oss.giussi.cappio.{ProcessId, Scheduler}

object URBLevel {

  def scheduler[P](nProcesses: Int, timeout: Int): Scheduler[URBMod[P]] = {
    val all = (0 to nProcesses).map(ProcessId).toSet
    Scheduler.init(all,UniformReliableBroadcast.init[P](all,timeout))
  }

  val urb = payloadRequest("Broadcast")({ case (_,msg) => URBBcast(msg) }) _

}

case class URBLevel(nProcesses: Int, timeout: Int) extends AbstractLevel[URBMod[String]](URBLevel.scheduler(nProcesses,timeout)) {

  override val reqTypes = List(
    URBLevel.urb,
    ActionSelection.crash
  )

  override val indicationPayload = ind => ind.payload.toString
}
