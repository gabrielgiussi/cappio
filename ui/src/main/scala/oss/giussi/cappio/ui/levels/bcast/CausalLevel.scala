package oss.giussi.cappio.ui.levels.bcast

import com.raquo.laminar.api.L._
import oss.giussi.cappio.{ProcessId, Scheduler}
import oss.giussi.cappio.impl.bcast.CausalOrderReliableBroadcast
import oss.giussi.cappio.impl.bcast.CausalOrderReliableBroadcast.{CRBBroadcast, CausalApp}
import oss.giussi.cappio.ui.ActionSelection
import oss.giussi.cappio.ui.levels.{AbstractLevel, Level}
import oss.giussi.cappio.ui.levels.Snapshot.Conditions

object CausalLevel {

  type ModLevel = CausalApp[String]

  val crb = ActionSelection.payloadRequest("Broadcast"){ case (_,payload) => CRBBroadcast(payload) } _

  val conditions: Conditions[ModLevel] = List()

  def scheduler(nProcesses: Int, timeout: Int): Scheduler[ModLevel] = {
    val all = (0 to nProcesses).map(ProcessId).toSet
    Scheduler.init(all,CausalOrderReliableBroadcast.app[String](all,timeout))
  }

  def apply(cond: Conditions[ModLevel])(nProcesses: Int, timeout: Int): Level[ModLevel] = new AbstractLevel[ModLevel](scheduler(nProcesses,timeout),cond) {
    override val indicationPayload = _.msg
    override val reqTypes = List(
      crb,
      ActionSelection.crash
    )
    override val shortDescription = div()

    override def title: String = "TODO"
  }

  val good = apply(List()) _
}