package oss.giussi.cappio.ui.levels.bcast

import com.raquo.laminar.api.L._
import oss.giussi.cappio.Scheduler.AutoDeliver
import oss.giussi.cappio.impl.CRDTApp.CRDTMod
import oss.giussi.cappio.impl.bcast.ReliableBroadcast
import oss.giussi.cappio.impl.bcast.ReliableBroadcast._
import oss.giussi.cappio.ui.ActionSelection
import oss.giussi.cappio.ui.levels.AbstractLevel
import oss.giussi.cappio.ui.levels.bcast.RBLevel.ModLevel
import oss.giussi.cappio.{ProcessId, Scheduler}
import shapeless.Inl

object RBLevel {
  type ModLevel = RBApp[String]

  def scheduler[P](nProcesses: Int, timeout: Int): Scheduler[ModLevel] = {
    val all = (0 to nProcesses).map(ProcessId).toSet
    val ad: AutoDeliver[ModLevel#Payload] = packet => packet.payload match {
      case Inl(_) => true
      case _ => false
    }
    Scheduler.withAutoDeliver[ModLevel](all,ReliableBroadcast.app[String](all,timeout),ad)
  }

  val rb = ActionSelection.payloadRequest("Broadcast")({ case (_,s) => RBBcast(s)}) _

}

case class RBLevel(nProcesses: Int, timeout: Int) extends AbstractLevel[ModLevel](RBLevel.scheduler(nProcesses,timeout)) {

  override val indicationPayload = ind => ind.payload.msg.toString

  override val reqTypes = List(
    RBLevel.rb,
    ActionSelection.crash
  )

  override val shortDescription = div()

}
