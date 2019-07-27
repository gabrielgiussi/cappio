package oss.giussi.cappio.impl.bcast

import oss.giussi.cappio.Messages.ProcessLocal
import oss.giussi.cappio._
import oss.giussi.cappio.impl.bcast.BestEffortBroadcast.{BEBState, BebMod}
import oss.giussi.cappio.impl.bcast.UniformReliableBroadcast.Payload
import oss.giussi.cappio.impl.net.PerfectLink.{PLDeliver, PLSend}
import oss.giussi.cappio.impl.net.PerfectLinkBeta
import oss.giussi.cappio.impl.net.PerfectLinkBeta.PLModule



object BestEffortBroadcast {

  type BebMod = ModS[PLModule] {
    type Req = BebBcast
    type S = BEBState
    type Ind = BebDeliver
  }

  case class BebBcast(payload: Payload, instance: Instance)

  case class BebDeliver(from: ProcessId, payload: Payload)

  object BEBState {
    def init(timeout: Int) = BEBState(PerfectLinkBeta.init(timeout))
  }

  case class BEBState(module: Module[PLModule]) extends StateWithModule[PLModule, BEBState] {
    override def updateModule(m: Module[PLModule]): BEBState = copy(m)
  }

  def init(self: ProcessId,all:Set[ProcessId], timeout: Int): BestEffortBroadcast = BestEffortBroadcast(self,all,BEBState.init(timeout))

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

case class BestEffortBroadcast(self: ProcessId, all: Set[ProcessId], state: BEBState) extends AbstractModule[BebMod,PLModule] {
  override def copyModule(s: BEBState) = copy(state = s)

  override val processLocal: PLocal = BestEffortBroadcast.processLocal(self,all)
}