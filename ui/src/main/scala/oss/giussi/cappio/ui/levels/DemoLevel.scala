package oss.giussi.cappio.ui.levels

import com.raquo.laminar.api.L._
import oss.giussi.cappio.impl.AppState
import oss.giussi.cappio.{ProcessId, Scheduler}
import oss.giussi.cappio.impl.AppState.AppMod2
import oss.giussi.cappio.impl.net.PerfectLink
import oss.giussi.cappio.impl.net.PerfectLink.{PLModule, PLSend}
import oss.giussi.cappio.ui.ActionSelection
import oss.giussi.cappio.ui.levels.Snapshot.Conditions

object DemoLevel {

  type ModLevel = AppMod2[String,PLModule[String]]

  val selection = ActionSelection.fromToPayloadRequest("Enviar"){ case (from,to,payload) => PLSend.external(from,to,payload) } _

  val conditions: Conditions[ModLevel] = List()

  def scheduler(nProcesses: Int, timeout: Int): Scheduler[ModLevel] = {
    val all = (0 to nProcesses).map(ProcessId).toSet
    val app = AppState.app2[String,PLModule[String]](PerfectLink[String](timeout), (state, ind) => state.update(ind.packet.payload))
    Scheduler.init(all,_ => app)
  }

  def apply(cond: Conditions[ModLevel])(nProcesses: Int, timeout: Int): Level[ModLevel] = new AbstractLevel[ModLevel](scheduler(nProcesses,timeout),cond) {
    override val indicationPayload = _.packet.payload
    override val reqTypes = List(
      selection,
      ActionSelection.crash
    )
    override val shortDescription = div(
      ""
    )
  }

  val good = DemoLevel(conditions) _
}
