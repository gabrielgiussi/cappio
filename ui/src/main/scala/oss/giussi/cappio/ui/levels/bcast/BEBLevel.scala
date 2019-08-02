package oss.giussi.cappio.ui.levels.bcast

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.html.Element
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BebBcast, BebDeliver, BebMod}
import oss.giussi.cappio.ui.ActionSelection
import oss.giussi.cappio.ui.ActionSelection.{Inputs, payloadRequest}
import oss.giussi.cappio.ui.levels.AbstractLevel
import oss.giussi.cappio.{ProcessId, Scheduler}

object BEBLevel {

  def scheduler[P](nProcesses: Int, timeout: Int): Scheduler[BebMod[P]] = {
    val all = (0 to nProcesses).map(ProcessId).toSet
    Scheduler.init(all,BestEffortBroadcast.init[P](all,timeout))
  }

  val beb = payloadRequest({ case (_,s) => BebBcast(s)}) _

}

case class BEBLevel(nProcesses: Int, timeout: Int) extends AbstractLevel[BebMod[String], String](BEBLevel.scheduler(nProcesses,timeout)) {

  override val reqTypes: List[Inputs[BebBcast[String]]] = List(
    BEBLevel.beb,
    ActionSelection.crash
  )

  override def requestPayload(req: BebBcast[String]): String = req.payload.msg.toString

  override def indicationPayload(ind: BebDeliver[String]): String = ind.payload.msg.toString
}
