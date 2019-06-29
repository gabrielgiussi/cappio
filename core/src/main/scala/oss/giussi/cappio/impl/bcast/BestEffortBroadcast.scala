package oss.giussi.cappio.impl.bcast

import oss.giussi.cappio.Messages.ProcessLocal
import oss.giussi.cappio._
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BEBState, BebBcast, BebDeliver}
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.Payload
import oss.giussi.cappio.impl.net.PerfectLink
import oss.giussi.cappio.impl.net.PerfectLink.{PLDeliver, PLSend, PerfectLinkState}

object BestEffortBroadcast {

  case class BebBcast(payload: Payload, instance: Instance)

  case class BebDeliver(from: ProcessId, payload: Payload)

  object BEBState {
    def init(timeout: Int) = BEBState(PerfectLink.init(timeout))
  }

  case class BEBState(module: Module[PLSend, PerfectLinkState, PLDeliver]) extends StateWithModule[PLSend, PerfectLinkState, PLDeliver, BEBState] {
    override def updateModule(m: Module[PLSend, PerfectLinkState, PLDeliver]): BEBState = copy(m)
  }

  def init(self: ProcessId,all:Set[ProcessId], timeout: Int) = BestEffortBroadcast(self,all,BEBState.init(timeout))

  def processLocal(self: ProcessId, all: Set[ProcessId]): ProcessLocal[BebBcast, BEBState, BebDeliver, PLSend, PLDeliver] = {
    import Messages._
    (msg,state) => msg match {
      case Tick => LocalStep.withState(state)
      case PublicRequest(BebBcast(Payload(id,msg), instance)) =>
        val req = all.map(to => LocalRequest(PLSend(Packet(id,msg, self, to, instance))))
        LocalStep.withRequests(req, state)
      case LocalIndication(PLDeliver(Packet(id, payload, from, _, _))) => LocalStep.withIndications(Set(BebDeliver(from, Payload(id,payload))), state)
    }
  }
}

case class BestEffortBroadcast(self: ProcessId, all: Set[ProcessId], state: BEBState) extends AbstractModule[BebBcast, BEBState, BebDeliver, PLSend, PerfectLinkState, PLDeliver] {
  import Messages._
  override def copyModule(s: BEBState): AbstractModule[BebBcast, BEBState, BebDeliver, PLSend, PerfectLinkState, PLDeliver] = copy(state = s)

  def processLocal(l: LMsg, state: BEBState): LStep = l match {
    case Tick => LocalStep.withState(state)
    case PublicRequest(BebBcast(Payload(id,msg), instance)) =>
      val req = all.map(to => LocalRequest(PLSend(Packet(id,msg, self, to, instance))))
      LocalStep.withRequests(req, state)
    case LocalIndication(PLDeliver(Packet(id, payload, from, _, _))) => LocalStep.withIndications(Set(BebDeliver(from, Payload(id,payload))), state)
  }

  override val processLocal: PLocal = BestEffortBroadcast.processLocal(self,all)
}