package oss.giussi.cappio.ui.levels.bcast

import com.raquo.laminar.api.L._
import oss.giussi.cappio.Scheduler.AutoDeliver
import oss.giussi.cappio.impl.CRDTApp
import oss.giussi.cappio.{ProcessId, Scheduler}
import oss.giussi.cappio.impl.CRDTApp.{Add, CRDTMod, SetRequest}
import oss.giussi.cappio.ui.ActionSelection
import oss.giussi.cappio.ui.levels.{AbstractLevel, Level}
import oss.giussi.cappio.ui.levels.Snapshot.Conditions
import shapeless.Inl

object CRDTLevel {

  type ModLevel = CRDTMod

  val add = ActionSelection.payloadRequest[SetRequest]("Add"){ case (_,payload) => Add(payload) } _

  val conditions: Conditions[ModLevel] = List()

  def scheduler(nProcesses: Int, timeout: Int): Scheduler[ModLevel] = {
    val all = (0 to nProcesses).map(ProcessId).toSet
    val ad: AutoDeliver[CRDTMod#Payload] = packet => packet.payload match {
      case Inl(_) => true
      case _ => false
    }
    Scheduler.withAutoDeliver[CRDTMod](all,CRDTApp(all,timeout), ad)
  }

  def apply(cond: Conditions[ModLevel])(nProcesses: Int, timeout: Int): Level[ModLevel] = new AbstractLevel[ModLevel](scheduler(nProcesses,timeout),cond) {
    override val indicationPayload = _.toString
    override val reqTypes = List(
      add,
      ActionSelection.crash
    )
    override val shortDescription = div()

    override def title: String = "TODO"

  }

  val good = CRDTLevel(List()) _

}
