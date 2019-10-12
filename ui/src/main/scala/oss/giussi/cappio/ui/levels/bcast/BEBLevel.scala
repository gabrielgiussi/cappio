package oss.giussi.cappio.ui.levels.bcast

import oss.giussi.cappio.Conditions.Condition
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BebBcast, BebMod}
import oss.giussi.cappio.ui.{ActionSelection, Show}
import oss.giussi.cappio.ui.ActionSelection.{Inputs, payloadRequest}
import oss.giussi.cappio.ui.core.Dropped
import oss.giussi.cappio.ui.levels.{AbstractLevel, Snapshot}
import oss.giussi.cappio.ui.levels.Snapshot.Conditions
import oss.giussi.cappio.ui.levels.bcast.BEBLevel.ModLevel
import oss.giussi.cappio.{ConditionResult, IndicationFrom, ProcessId, Scheduler}

object BEBLevel {

  type ModLevel = BebMod[String]

  def scheduler[P](nProcesses: Int, timeout: Int): Scheduler[ModLevel] = {
    val all = (0 to nProcesses).map(ProcessId).toSet
    Scheduler.init(all,BestEffortBroadcast[String](all,timeout))
  }

  val beb = payloadRequest("Broadcast")({ case (_,s) => BebBcast(s)}) _

  import oss.giussi.cappio.ui.core.LevelConditions._
  import oss.giussi.cappio.Conditions._
  val conditions: Conditions[ModLevel] = List(

  )

  val simple = BEBLevel(List(
    processes(ALL_UP[ModLevel]) _,
    condition[Snapshot[ModLevel]]("Deliver to all","All processes should BebDeliver 'A'",snap => {
      if (snap.step.scheduler.processes.keys.toSet == snap.indications.filter(_.i.payload.msg == "A").map(_.p)) None else Some("Not all processes delivered 'A'")
    })
  )) _

  val broken = BEBLevel(List()) _

}

case class BEBLevel(cond: Conditions[ModLevel])(nProcesses: Int, timeout: Int) extends AbstractLevel[BebMod[String]](BEBLevel.scheduler(nProcesses,timeout), cond) {

  override val reqTypes: List[Inputs[BebBcast[String]]] = List(
    BEBLevel.beb,
    ActionSelection.crash
  )

  override def requestPayload(req: BebBcast[String]): String = req.payload.msg.toString

  override val indicationPayload = ind => ind.payload.msg.toString

}
