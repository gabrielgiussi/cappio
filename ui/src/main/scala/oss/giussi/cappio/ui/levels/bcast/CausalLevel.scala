package oss.giussi.cappio.ui.levels.bcast

import com.raquo.laminar.api.L._
import oss.giussi.cappio.impl.PhotosApp
import oss.giussi.cappio.impl.PhotosApp.AlbumCausal
import oss.giussi.cappio.{ProcessId, Scheduler}
import oss.giussi.cappio.impl.bcast.CausalOrderReliableBroadcast
import oss.giussi.cappio.impl.bcast.CausalOrderReliableBroadcast.{CRBBroadcast, CausalApp}
import oss.giussi.cappio.ui.ActionSelection
import oss.giussi.cappio.ui.levels.{AbstractLevel, Level}
import oss.giussi.cappio.ui.levels.Snapshot.Conditions

object CausalLevel {

  type ModLevel = AlbumCausal

  val crb = ActionSelection.payloadRequest("Broadcast"){ case (_,payload) => CRBBroadcast(payload) } _

  val conditions: Conditions[ModLevel] = List()

  def scheduler(nProcesses: Int, timeout: Int): Scheduler[ModLevel] = {
    val all = (0 to nProcesses).map(ProcessId).toSet
    Scheduler.init(all,PhotosApp.causal(all,timeout))
  }

  def apply(cond: Conditions[ModLevel])(nProcesses: Int, timeout: Int): Level[ModLevel] = new AbstractLevel[ModLevel](scheduler(nProcesses,timeout),cond) {
    override val indicationPayload = _ => "" // TODO
    override val reqTypes = List(
      ActionSelection.crash
    )
    override val shortDescription = div()

    override def title: String = "TODO"
  }

  val ok = apply(List()) _
}