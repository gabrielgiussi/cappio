package oss.giussi.cappio.ui.levels.bcast

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.html.Element
import oss.giussi.cappio.{Process, ProcessId, Scheduler}
import oss.giussi.cappio.impl.bcast.ReliableBroadcast
import oss.giussi.cappio.impl.bcast.ReliableBroadcast._
import oss.giussi.cappio.ui.levels.AbstractLevel

object RBLevel {
  def scheduler(nProcesses: Int, timeout: Int) = {
    val ids = (0 to nProcesses).map(ProcessId).toSet
    val processes = ids.map(p => Process(p,ReliableBroadcast.init(p,ids,timeout)))
    Scheduler.init(processes.toList)
  }
}

case class RBLevel(nProcesses: Int, timeout: Int) extends AbstractLevel[RBBcast, RBcastState, RBDeliver, String](RBLevel.scheduler(nProcesses,timeout)) {
  override def requestPayload(req: RBBcast): String = req.payload.msg.toString

  override def indicationPayload(ind: RBDeliver): String = ind.payload.msg.toString

  override def actionSel(obs: Observer[Option[RBBcast]])(processId: ProcessId): String => ReactiveHtmlElement[Element] = _ => input(

  )

  override val reqTypes: List[String] = List("rb-bcast")
}
