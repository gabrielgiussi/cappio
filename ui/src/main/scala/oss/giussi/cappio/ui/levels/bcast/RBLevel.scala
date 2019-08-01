package oss.giussi.cappio.ui.levels.bcast

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.html.Element
import oss.giussi.cappio.impl.bcast.ReliableBroadcast
import oss.giussi.cappio.impl.bcast.ReliableBroadcast._
import oss.giussi.cappio.ui.levels.AbstractLevel
import oss.giussi.cappio.{ProcessId, Scheduler}

object RBLevel {
  def scheduler[P](nProcesses: Int, timeout: Int): Scheduler[RBMod[P]] = {
    val all = (0 to nProcesses).map(ProcessId).toSet
    Scheduler.init(all,ReliableBroadcast.init[P](all,timeout))
  }
}

case class RBLevel(nProcesses: Int, timeout: Int) extends AbstractLevel[RBMod[String], String](RBLevel.scheduler(nProcesses,timeout)) {
  override def requestPayload(req: RBBcast[String]): String = req.payload.msg.toString

  override def indicationPayload(ind: RBDeliver[String]): String = ind.payload.msg.toString

  override def actionSel(obs: Observer[Option[RBBcast[String]]])(processId: ProcessId): String => ReactiveHtmlElement[Element] = _ => input(

  )

  override val reqTypes: List[String] = List("rb-bcast")
}
