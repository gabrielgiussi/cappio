package oss.giussi.cappio.ui.levels.bcast

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.html.Element
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.{Payload, URBBcast, URBDeliver, URBState}
import oss.giussi.cappio.ui.ActionSelection
import oss.giussi.cappio.ui.levels.AbstractLevel
import oss.giussi.cappio.{Process, ProcessId, Scheduler}

object URBLevel {

  def scheduler(nProcesses: Int, timeout: Int): Scheduler[URBBcast, URBState, URBDeliver] = {
    val ids = (0 to nProcesses).map(ProcessId).toSet
    val processes = ids.map(p => Process(p,UniformReliableBroadcast.init(p,ids,timeout)))
    Scheduler.init(processes.toList)
  }

}

case class URBLevel(nProcesses: Int, timeout: Int) extends AbstractLevel[URBBcast, URBState, URBDeliver, String](URBLevel.scheduler(nProcesses,timeout)) {

  override val reqTypes: List[String] = List("urb-bcast")

  override def actionSel(obs: Observer[Option[URBBcast]])(processId: ProcessId): String => ReactiveHtmlElement[Element] = _ => ActionSelection.payloadInput(
    obs.contramap(_.map(raw => URBBcast(Payload(msg = raw))))
  )

  override def requestPayload(req: URBBcast): String = req.payload.msg.toString

  override def indicationPayload(ind: URBDeliver): String = ind.payload.toString
}
