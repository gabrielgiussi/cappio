package oss.giussi.cappio.ui.levels.bcast

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.html.Element
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BebBcast, BebDeliver, BebMod}
import oss.giussi.cappio.ui.ActionSelection
import oss.giussi.cappio.ui.levels.AbstractLevel
import oss.giussi.cappio.{ProcessId, Scheduler}

object BEBLevel {

  def scheduler[P](nProcesses: Int, timeout: Int): Scheduler[BebMod[P]] = {
    val all = (0 to nProcesses).map(ProcessId).toSet
    Scheduler.init(all,BestEffortBroadcast.init[P](all,timeout))
  }


}

case class BEBLevel(nProcesses: Int, timeout: Int) extends AbstractLevel[BebMod[String], String](BEBLevel.scheduler(nProcesses,timeout)) {

  override def actionSel(obs: Observer[Option[BebBcast[String]]])(processId: ProcessId): String => ReactiveHtmlElement[Element] = _ =>
    ActionSelection.payloadInput(obs.contramap(_.map(raw => BebBcast(raw))))

  override val reqTypes: List[String] = List("beb")

  override def requestPayload(req: BebBcast[String]): String = req.payload.msg.toString

  override def indicationPayload(ind: BebDeliver[String]): String = ind.payload.msg.toString
}
