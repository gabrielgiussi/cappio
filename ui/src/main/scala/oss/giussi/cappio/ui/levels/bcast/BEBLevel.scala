package oss.giussi.cappio.ui.levels.bcast

import oss.giussi.cappio.impl.bcast.BestEffortBroadcast
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BebBcast, BebMod}
import oss.giussi.cappio.ui.ActionSelection
import oss.giussi.cappio.ui.ActionSelection.{Inputs, payloadRequest}
import oss.giussi.cappio.ui.levels.AbstractLevel
import oss.giussi.cappio.{ProcessId, Scheduler}

object BEBLevel {

  type ModLevel = BebMod[String]

  def scheduler[P](nProcesses: Int, timeout: Int): Scheduler[ModLevel] = {
    val all = (0 to nProcesses).map(ProcessId).toSet
    Scheduler.init(all,BestEffortBroadcast.init[String](all,timeout))
  }

  val beb = payloadRequest("Broadcast")({ case (_,s) => BebBcast(s)}) _

  import oss.giussi.cappio.Conditions._
  val conditions: List[Condition[Scheduler[ModLevel]]] = List(
    processes(ALL_UP[ModLevel]) _
  )

}

case class BEBLevel(nProcesses: Int, timeout: Int) extends AbstractLevel[BebMod[String]](BEBLevel.scheduler(nProcesses,timeout), BEBLevel.conditions) {

  override val reqTypes: List[Inputs[BebBcast[String]]] = List(
    BEBLevel.beb,
    ActionSelection.crash
  )

  override def requestPayload(req: BebBcast[String]): String = req.payload.msg.toString

  override val indicationPayload = ind => ind.payload.msg.toString
}
