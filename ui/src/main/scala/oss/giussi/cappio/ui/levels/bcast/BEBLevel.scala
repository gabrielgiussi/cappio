package oss.giussi.cappio.ui.levels.bcast

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.html.Element
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BEBState, BebBcast, BebDeliver}
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.Payload
import oss.giussi.cappio.ui.ActionSelection
import oss.giussi.cappio.ui.levels.AbstractLevel
import oss.giussi.cappio.{Instance, Process, ProcessId, Scheduler}

object BEBLevel {

  def scheduler(nProcesses: Int, timeout: Int): Scheduler[BebBcast, BEBState, BebDeliver] = {
    val ids = (0 to nProcesses).map(ProcessId).toSet
    val processes = ids.map(p => Process(p,BestEffortBroadcast.init(p,ids,timeout)))
    Scheduler.init(processes.toList)
  }


}

case class BEBLevel(nProcesses: Int, timeout: Int) extends AbstractLevel[BebBcast,BEBState,BebDeliver, String](BEBLevel.scheduler(nProcesses,timeout)) {

  override def actionSel(obs: Observer[Option[BebBcast]])(processId: ProcessId): String => ReactiveHtmlElement[Element] = _ =>
    ActionSelection.payloadInput(obs.contramap(_.map(raw => BebBcast(Payload(msg = raw),Instance.ANY))))

  override val reqTypes: List[String] = List("beb")

  override def requestPayload(req: BebBcast): String = req.payload.msg.toString

  override def indicationPayload(ind: BebDeliver): String = ind.payload.msg.toString
}
