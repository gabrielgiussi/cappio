package oss.giussi.cappio.ui.levels.bcast

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.html.Element
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.{URBBcast, URBDeliver, URBMod}
import oss.giussi.cappio.ui.ActionSelection
import oss.giussi.cappio.ui.levels.AbstractLevel
import oss.giussi.cappio.{ProcessId, Scheduler}

object URBLevel {

  def scheduler[P](nProcesses: Int, timeout: Int): Scheduler[URBMod[P]] = {
    val all = (0 to nProcesses).map(ProcessId).toSet
    Scheduler.init(all,UniformReliableBroadcast.init[P](all,timeout))
  }

}

case class URBLevel(nProcesses: Int, timeout: Int) extends AbstractLevel[URBMod[String], String](URBLevel.scheduler(nProcesses,timeout)) {

  override val reqTypes: List[String] = List("urb-bcast")

  override def actionSel(obs: Observer[Option[URBBcast[String]]])(processId: ProcessId): String => ReactiveHtmlElement[Element] = _ => ActionSelection.payloadInput(
    obs.contramap(_.map(raw => URBBcast(raw)))
  )

  override def requestPayload(req: URBBcast[String]): String = req.payload.msg.toString

  override def indicationPayload(ind: URBDeliver[String]): String = ind.payload.toString
}
