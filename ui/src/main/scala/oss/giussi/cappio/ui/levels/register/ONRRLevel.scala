package oss.giussi.cappio.ui.levels.register

import oss.giussi.cappio.Conditions.Condition
import oss.giussi.cappio.{ProcessId, Scheduler}
import oss.giussi.cappio.impl.register.OneNRegularRegister
import oss.giussi.cappio.impl.register.OneNRegularRegister.{ONRRInd, ONRRMod, ONRRRead, ONRRReadReturn, ONRRReq, ONRRWrite, ONRRWriteReturn}
import oss.giussi.cappio.ui.ActionSelection
import oss.giussi.cappio.ui.ActionSelection.{Inputs, RequestWrapper}
import oss.giussi.cappio.ui.levels.AbstractLevel
import oss.giussi.cappio.ui.levels.register.ONRRLevel.ONRR

object ONRRLevel {
  type ONRR = ONRRMod[String]

  def scheduler(nProcesses: Int, timeout: Int): Scheduler[ONRR] = {
    val all = (0 to nProcesses).map(ProcessId).toSet
    val N = all.size
    Scheduler.init(all,OneNRegularRegister.init[String](N,timeout,all))
  }

  val conditions: List[Condition[Scheduler[ONRR]]] = List()

  val onrrRead = ActionSelection.noPayloadRequest[ONRRReq[String]]("Read")(_ => RequestWrapper(ONRRRead)) _
  val onrrWrite = ActionSelection.payloadRequest[ONRRReq[String]]("Write")({ case (_,v) => ONRRWrite(v) }) _
}

case class ONRRLevel(nProcesses: Int, timeout: Int) extends AbstractLevel[ONRR](ONRRLevel.scheduler(nProcesses,timeout),ONRRLevel.conditions) {
  import ONRRLevel._

  override def requestPayload(req: ONRRReq[String]): String = req match {
    case ONRRRead => "read"
    case ONRRWrite(v) => s"write $v"
  }

  override val indicationPayload: ONRRInd[String] => String = {
    case ONRRReadReturn(v) => s"read $v"
    case ONRRWriteReturn => "write ok"
  }

  override val reqTypes: List[Inputs[Req]] = List(
    onrrRead,
    onrrWrite,
    ActionSelection.crash
  )

  /*
  broken
  - split (laminar)
  - P0.send and P1.indication arrows are head to head in the diagram
  - InTransitPacket show payloads with Inl(Inl(..
  - ONRR delivers are being unselected
   */
}
